/* Bounds-checking of reads and writes to memory regions.
   Copyright (C) 2019-2022 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "make-unique.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "diagnostic-core.h"
#include "diagnostic-metadata.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/region-model.h"

#if ENABLE_ANALYZER

namespace ana {

/* Abstract base class for all out-of-bounds warnings with concrete values.  */

class out_of_bounds : public pending_diagnostic_subclass<out_of_bounds>
{
public:
  out_of_bounds (const region *reg, tree diag_arg,
		 byte_range out_of_bounds_range)
  : m_reg (reg), m_diag_arg (diag_arg),
    m_out_of_bounds_range (out_of_bounds_range)
  {}

  const char *get_kind () const final override
  {
    return "out_of_bounds_diagnostic";
  }

  bool operator== (const out_of_bounds &other) const
  {
    return m_reg == other.m_reg
	   && m_out_of_bounds_range == other.m_out_of_bounds_range
	   && pending_diagnostic::same_tree_p (m_diag_arg, other.m_diag_arg);
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_out_of_bounds;
  }

  void mark_interesting_stuff (interesting_t *interest) final override
  {
    interest->add_region_creation (m_reg);
  }

protected:
  const region *m_reg;
  tree m_diag_arg;
  byte_range m_out_of_bounds_range;
};

/* Abstract subclass to complaing about out-of-bounds
   past the end of the buffer.  */

class past_the_end : public out_of_bounds
{
public:
  past_the_end (const region *reg, tree diag_arg, byte_range range,
		tree byte_bound)
  : out_of_bounds (reg, diag_arg, range), m_byte_bound (byte_bound)
  {}

  bool operator== (const past_the_end &other) const
  {
    return out_of_bounds::operator== (other)
	   && pending_diagnostic::same_tree_p (m_byte_bound,
					       other.m_byte_bound);
  }

  label_text
  describe_region_creation_event (const evdesc::region_creation &ev) final
  override
  {
    if (m_byte_bound && TREE_CODE (m_byte_bound) == INTEGER_CST)
      return ev.formatted_print ("capacity is %E bytes", m_byte_bound);

    return label_text ();
  }

protected:
  tree m_byte_bound;
};

/* Concrete subclass to complain about buffer overflows.  */

class buffer_overflow : public past_the_end
{
public:
  buffer_overflow (const region *reg, tree diag_arg,
		   byte_range range, tree byte_bound)
  : past_the_end (reg, diag_arg, range, byte_bound)
  {}

  bool emit (rich_location *rich_loc) final override
  {
    diagnostic_metadata m;
    bool warned;
    switch (m_reg->get_memory_space ())
      {
      default:
	m.add_cwe (787);
	warned = warning_meta (rich_loc, m, get_controlling_option (),
			       "buffer overflow");
	break;
      case MEMSPACE_STACK:
	m.add_cwe (121);
	warned = warning_meta (rich_loc, m, get_controlling_option (),
			       "stack-based buffer overflow");
	break;
      case MEMSPACE_HEAP:
	m.add_cwe (122);
	warned = warning_meta (rich_loc, m, get_controlling_option (),
			       "heap-based buffer overflow");
	break;
      }

    if (warned)
      {
	char num_bytes_past_buf[WIDE_INT_PRINT_BUFFER_SIZE];
	print_dec (m_out_of_bounds_range.m_size_in_bytes,
		   num_bytes_past_buf, UNSIGNED);
	if (m_diag_arg)
	  inform (rich_loc->get_loc (), "write is %s bytes past the end"
					" of %qE", num_bytes_past_buf,
						   m_diag_arg);
	else
	  inform (rich_loc->get_loc (), "write is %s bytes past the end"
					"of the region",
					num_bytes_past_buf);
      }

    return warned;
  }

  label_text describe_final_event (const evdesc::final_event &ev)
  final override
  {
    byte_size_t start = m_out_of_bounds_range.get_start_byte_offset ();
    byte_size_t end = m_out_of_bounds_range.get_last_byte_offset ();
    char start_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (start, start_buf, SIGNED);
    char end_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (end, end_buf, SIGNED);

    if (start == end)
      {
	if (m_diag_arg)
	  return ev.formatted_print ("out-of-bounds write at byte %s but %qE"
				     " ends at byte %E", start_buf, m_diag_arg,
							 m_byte_bound);
	return ev.formatted_print ("out-of-bounds write at byte %s but region"
				   " ends at byte %E", start_buf,
						       m_byte_bound);
      }
    else
      {
	if (m_diag_arg)
	  return ev.formatted_print ("out-of-bounds write from byte %s till"
				     " byte %s but %qE ends at byte %E",
				     start_buf, end_buf, m_diag_arg,
				     m_byte_bound);
	return ev.formatted_print ("out-of-bounds write from byte %s till"
				   " byte %s but region ends at byte %E",
				   start_buf, end_buf, m_byte_bound);
      }
  }
};

/* Concrete subclass to complain about buffer overreads.  */

class buffer_overread : public past_the_end
{
public:
  buffer_overread (const region *reg, tree diag_arg,
		   byte_range range, tree byte_bound)
  : past_the_end (reg, diag_arg, range, byte_bound)
  {}

  bool emit (rich_location *rich_loc) final override
  {
    diagnostic_metadata m;
    m.add_cwe (126);
    bool warned = warning_meta (rich_loc, m, get_controlling_option (),
				"buffer overread");

    if (warned)
      {
	char num_bytes_past_buf[WIDE_INT_PRINT_BUFFER_SIZE];
	print_dec (m_out_of_bounds_range.m_size_in_bytes,
		   num_bytes_past_buf, UNSIGNED);
	if (m_diag_arg)
	  inform (rich_loc->get_loc (), "read is %s bytes past the end"
					" of %qE", num_bytes_past_buf,
						    m_diag_arg);
	else
	  inform (rich_loc->get_loc (), "read is %s bytes past the end"
					"of the region",
					num_bytes_past_buf);
      }

    return warned;
  }

  label_text describe_final_event (const evdesc::final_event &ev)
  final override
  {
    byte_size_t start = m_out_of_bounds_range.get_start_byte_offset ();
    byte_size_t end = m_out_of_bounds_range.get_last_byte_offset ();
    char start_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (start, start_buf, SIGNED);
    char end_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (end, end_buf, SIGNED);

    if (start == end)
      {
	if (m_diag_arg)
	  return ev.formatted_print ("out-of-bounds read at byte %s but %qE"
				     " ends at byte %E", start_buf, m_diag_arg,
							 m_byte_bound);
	return ev.formatted_print ("out-of-bounds read at byte %s but region"
				   " ends at byte %E", start_buf,
						       m_byte_bound);
      }
    else
      {
	if (m_diag_arg)
	  return ev.formatted_print ("out-of-bounds read from byte %s till"
				     " byte %s but %qE ends at byte %E",
				     start_buf, end_buf, m_diag_arg,
				     m_byte_bound);
	return ev.formatted_print ("out-of-bounds read from byte %s till"
				   " byte %s but region ends at byte %E",
				   start_buf, end_buf, m_byte_bound);
      }
  }
};

/* Concrete subclass to complain about buffer underflows.  */

class buffer_underflow : public out_of_bounds
{
public:
  buffer_underflow (const region *reg, tree diag_arg, byte_range range)
  : out_of_bounds (reg, diag_arg, range)
  {}

  bool emit (rich_location *rich_loc) final override
  {
    diagnostic_metadata m;
    m.add_cwe (124);
    return warning_meta (rich_loc, m, get_controlling_option (),
			 "buffer underflow");
  }

  label_text describe_final_event (const evdesc::final_event &ev)
  final override
  {
    byte_size_t start = m_out_of_bounds_range.get_start_byte_offset ();
    byte_size_t end = m_out_of_bounds_range.get_last_byte_offset ();
    char start_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (start, start_buf, SIGNED);
    char end_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (end, end_buf, SIGNED);

    if (start == end)
      {
	if (m_diag_arg)
	  return ev.formatted_print ("out-of-bounds write at byte %s but %qE"
				     " starts at byte 0", start_buf,
							  m_diag_arg);
	return ev.formatted_print ("out-of-bounds write at byte %s but region"
				   " starts at byte 0", start_buf);
      }
    else
      {
	if (m_diag_arg)
	  return ev.formatted_print ("out-of-bounds write from byte %s till"
				     " byte %s but %qE starts at byte 0",
				     start_buf, end_buf, m_diag_arg);
	return ev.formatted_print ("out-of-bounds write from byte %s till"
				   " byte %s but region starts at byte 0",
				   start_buf, end_buf);;
      }
  }
};

/* Concrete subclass to complain about buffer underreads.  */

class buffer_underread : public out_of_bounds
{
public:
  buffer_underread (const region *reg, tree diag_arg, byte_range range)
  : out_of_bounds (reg, diag_arg, range)
  {}

  bool emit (rich_location *rich_loc) final override
  {
    diagnostic_metadata m;
    m.add_cwe (127);
    return warning_meta (rich_loc, m, get_controlling_option (),
			 "buffer underread");
  }

  label_text describe_final_event (const evdesc::final_event &ev)
  final override
  {
    byte_size_t start = m_out_of_bounds_range.get_start_byte_offset ();
    byte_size_t end = m_out_of_bounds_range.get_last_byte_offset ();
    char start_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (start, start_buf, SIGNED);
    char end_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (end, end_buf, SIGNED);

    if (start == end)
      {
	if (m_diag_arg)
	  return ev.formatted_print ("out-of-bounds read at byte %s but %qE"
				     " starts at byte 0", start_buf,
							  m_diag_arg);
	return ev.formatted_print ("out-of-bounds read at byte %s but region"
				  " starts at byte 0", start_buf);
      }
    else
      {
	if (m_diag_arg)
	  return ev.formatted_print ("out-of-bounds read from byte %s till"
				     " byte %s but %qE starts at byte 0",
				     start_buf, end_buf, m_diag_arg);
	return ev.formatted_print ("out-of-bounds read from byte %s till"
				   " byte %s but region starts at byte 0",
				   start_buf, end_buf);;
      }
  }
};

/* Abstract class to complain about out-of-bounds read/writes where
   the values are symbolic.  */

class symbolic_past_the_end
  : public pending_diagnostic_subclass<symbolic_past_the_end>
{
public:
  symbolic_past_the_end (const region *reg, tree diag_arg, tree offset,
			 tree num_bytes, tree capacity)
  : m_reg (reg), m_diag_arg (diag_arg), m_offset (offset),
    m_num_bytes (num_bytes), m_capacity (capacity)
  {}

  const char *get_kind () const final override
  {
    return "symbolic_past_the_end";
  }

  bool operator== (const symbolic_past_the_end &other) const
  {
    return m_reg == other.m_reg
	   && pending_diagnostic::same_tree_p (m_diag_arg, other.m_diag_arg)
	   && pending_diagnostic::same_tree_p (m_offset, other.m_offset)
	   && pending_diagnostic::same_tree_p (m_num_bytes, other.m_num_bytes)
	   && pending_diagnostic::same_tree_p (m_capacity, other.m_capacity);
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_out_of_bounds;
  }

  void mark_interesting_stuff (interesting_t *interest) final override
  {
    interest->add_region_creation (m_reg);
  }

  label_text
  describe_region_creation_event (const evdesc::region_creation &ev) final
  override
  {
    if (m_capacity)
      return ev.formatted_print ("capacity is %qE bytes", m_capacity);

    return label_text ();
  }

  label_text
  describe_final_event (const evdesc::final_event &ev) final override
  {
    const char *byte_str;
    if (pending_diagnostic::same_tree_p (m_num_bytes, integer_one_node))
      byte_str = "byte";
    else
      byte_str = "bytes";

    if (m_offset)
      {
	if (m_num_bytes && TREE_CODE (m_num_bytes) == INTEGER_CST)
	  {
	    if (m_diag_arg)
	      return ev.formatted_print ("%s of %E %s at offset %qE"
					 " exceeds %qE", m_dir_str,
					 m_num_bytes, byte_str,
					 m_offset, m_diag_arg);
	    else
	      return ev.formatted_print ("%s of %E %s at offset %qE"
					 " exceeds the buffer", m_dir_str,
					 m_num_bytes, byte_str, m_offset);
	  }
	else if (m_num_bytes)
	  {
	    if (m_diag_arg)
	      return ev.formatted_print ("%s of %qE %s at offset %qE"
					 " exceeds %qE", m_dir_str,
					 m_num_bytes, byte_str,
					 m_offset, m_diag_arg);
	    else
	      return ev.formatted_print ("%s of %qE %s at offset %qE"
					 " exceeds the buffer", m_dir_str,
					 m_num_bytes, byte_str, m_offset);
	  }
	else
	  {
	    if (m_diag_arg)
	      return ev.formatted_print ("%s at offset %qE exceeds %qE",
					 m_dir_str, m_offset, m_diag_arg);
	    else
	      return ev.formatted_print ("%s at offset %qE exceeds the"
					 " buffer", m_dir_str, m_offset);
	  }
      }
    if (m_diag_arg)
      return ev.formatted_print ("out-of-bounds %s on %qE",
				 m_dir_str, m_diag_arg);
    return ev.formatted_print ("out-of-bounds %s", m_dir_str);
  }

protected:
  const region *m_reg;
  tree m_diag_arg;
  tree m_offset;
  tree m_num_bytes;
  tree m_capacity;
  const char *m_dir_str;
};

/* Concrete subclass to complain about overflows with symbolic values.  */

class symbolic_buffer_overflow : public symbolic_past_the_end
{
public:
  symbolic_buffer_overflow (const region *reg, tree diag_arg, tree offset,
			    tree num_bytes, tree capacity)
  : symbolic_past_the_end (reg, diag_arg, offset, num_bytes, capacity)
  {
    m_dir_str = "write";
  }

  bool emit (rich_location *rich_loc) final override
  {
    diagnostic_metadata m;
    switch (m_reg->get_memory_space ())
      {
      default:
	m.add_cwe (787);
	return warning_meta (rich_loc, m, get_controlling_option (),
			     "buffer overflow");
      case MEMSPACE_STACK:
	m.add_cwe (121);
	return warning_meta (rich_loc, m, get_controlling_option (),
			     "stack-based buffer overflow");
      case MEMSPACE_HEAP:
	m.add_cwe (122);
	return warning_meta (rich_loc, m, get_controlling_option (),
			     "heap-based buffer overflow");
      }
  }
};

/* Concrete subclass to complain about overreads with symbolic values.  */

class symbolic_buffer_overread : public symbolic_past_the_end
{
public:
  symbolic_buffer_overread (const region *reg, tree diag_arg, tree offset,
			    tree num_bytes, tree capacity)
  : symbolic_past_the_end (reg, diag_arg, offset, num_bytes, capacity)
  {
    m_dir_str = "read";
  }

  bool emit (rich_location *rich_loc) final override
  {
    diagnostic_metadata m;
    m.add_cwe (126);
    return warning_meta (rich_loc, m, get_controlling_option (),
			 "buffer overread");
  }
};

/* Check whether an access is past the end of the BASE_REG.  */

void
region_model::check_symbolic_bounds (const region *base_reg,
				     const svalue *sym_byte_offset,
				     const svalue *num_bytes_sval,
				     const svalue *capacity,
				     enum access_direction dir,
				     region_model_context *ctxt) const
{
  gcc_assert (ctxt);

  const svalue *next_byte
    = m_mgr->get_or_create_binop (num_bytes_sval->get_type (), PLUS_EXPR,
				  sym_byte_offset, num_bytes_sval);

  if (eval_condition (next_byte, GT_EXPR, capacity).is_true ())
    {
      tree diag_arg = get_representative_tree (base_reg);
      tree offset_tree = get_representative_tree (sym_byte_offset);
      tree num_bytes_tree = get_representative_tree (num_bytes_sval);
      tree capacity_tree = get_representative_tree (capacity);
      switch (dir)
	{
	default:
	  gcc_unreachable ();
	  break;
	case DIR_READ:
	  ctxt->warn (make_unique<symbolic_buffer_overread> (base_reg,
							     diag_arg,
							     offset_tree,
							     num_bytes_tree,
							     capacity_tree));
	  break;
	case DIR_WRITE:
	  ctxt->warn (make_unique<symbolic_buffer_overflow> (base_reg,
							     diag_arg,
							     offset_tree,
							     num_bytes_tree,
							     capacity_tree));
	  break;
	}
    }
}

static tree
maybe_get_integer_cst_tree (const svalue *sval)
{
  tree cst_tree = sval->maybe_get_constant ();
  if (cst_tree && TREE_CODE (cst_tree) == INTEGER_CST)
    return cst_tree;

  return NULL_TREE;
}

/* May complain when the access on REG is out-of-bounds.  */

void
region_model::check_region_bounds (const region *reg,
				   enum access_direction dir,
				   region_model_context *ctxt) const
{
  gcc_assert (ctxt);

  /* Get the offset.  */
  region_offset reg_offset = reg->get_offset (m_mgr);
  const region *base_reg = reg_offset.get_base_region ();

  /* Bail out on symbolic regions.
     (e.g. because the analyzer did not see previous offsets on the latter,
     it might think that a negative access is before the buffer).  */
  if (base_reg->symbolic_p ())
    return;

  /* Find out how many bytes were accessed.  */
  const svalue *num_bytes_sval = reg->get_byte_size_sval (m_mgr);
  tree num_bytes_tree = maybe_get_integer_cst_tree (num_bytes_sval);
  /* Bail out if 0 bytes are accessed.  */
  if (num_bytes_tree && zerop (num_bytes_tree))
    return;

  /* Get the capacity of the buffer.  */
  const svalue *capacity = get_capacity (base_reg);
  tree cst_capacity_tree = maybe_get_integer_cst_tree (capacity);

  /* The constant offset from a pointer is represented internally as a sizetype
     but should be interpreted as a signed value here.  The statement below
     converts the offset from bits to bytes and then to a signed integer with
     the same precision the sizetype has on the target system.

     For example, this is needed for out-of-bounds-3.c test1 to pass when
     compiled with a 64-bit gcc build targeting 32-bit systems.  */
  byte_offset_t offset;
  if (!reg_offset.symbolic_p ())
    offset = wi::sext (reg_offset.get_bit_offset () >> LOG2_BITS_PER_UNIT,
		       TYPE_PRECISION (size_type_node));

  /* If either the offset or the number of bytes accessed are symbolic,
     we have to reason about symbolic values.  */
  if (reg_offset.symbolic_p () || !num_bytes_tree)
    {
      const svalue* byte_offset_sval;
      if (!reg_offset.symbolic_p ())
	{
	  tree offset_tree = wide_int_to_tree (integer_type_node, offset);
	  byte_offset_sval
	    = m_mgr->get_or_create_constant_svalue (offset_tree);
	}
      else
	byte_offset_sval = reg_offset.get_symbolic_byte_offset ();
      check_symbolic_bounds (base_reg, byte_offset_sval, num_bytes_sval,
			     capacity, dir, ctxt);
      return;
    }

  /* Otherwise continue to check with concrete values.  */
  byte_range out (0, 0);
  /* NUM_BYTES_TREE should always be interpreted as unsigned.  */
  byte_offset_t num_bytes_unsigned = wi::to_offset (num_bytes_tree);
  byte_range read_bytes (offset, num_bytes_unsigned);
  /* If read_bytes has a subset < 0, we do have an underflow.  */
  if (read_bytes.falls_short_of_p (0, &out))
    {
      tree diag_arg = get_representative_tree (base_reg);
      switch (dir)
	{
	default:
	  gcc_unreachable ();
	  break;
	case DIR_READ:
	  ctxt->warn (make_unique<buffer_underread> (reg, diag_arg, out));
	  break;
	case DIR_WRITE:
	  ctxt->warn (make_unique<buffer_underflow> (reg, diag_arg, out));
	  break;
	}
    }

  /* For accesses past the end, we do need a concrete capacity.  No need to
     do a symbolic check here because the inequality check does not reason
     whether constants are greater than symbolic values.  */
  if (!cst_capacity_tree)
    return;

  byte_range buffer (0, wi::to_offset (cst_capacity_tree));
  /* If READ_BYTES exceeds BUFFER, we do have an overflow.  */
  if (read_bytes.exceeds_p (buffer, &out))
    {
      tree byte_bound = wide_int_to_tree (size_type_node,
					  buffer.get_next_byte_offset ());
      tree diag_arg = get_representative_tree (base_reg);

      switch (dir)
	{
	default:
	  gcc_unreachable ();
	  break;
	case DIR_READ:
	  ctxt->warn (make_unique<buffer_overread> (reg, diag_arg,
						    out, byte_bound));
	  break;
	case DIR_WRITE:
	  ctxt->warn (make_unique<buffer_overflow> (reg, diag_arg,
						    out, byte_bound));
	  break;
	}
    }
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
