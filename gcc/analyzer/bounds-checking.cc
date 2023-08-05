/* Bounds-checking of reads and writes to memory regions.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.

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
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "make-unique.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "intl.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "diagnostic-core.h"
#include "diagnostic-metadata.h"
#include "diagnostic-diagram.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/region-model.h"
#include "analyzer/checker-event.h"
#include "analyzer/checker-path.h"
#include "analyzer/access-diagram.h"

#if ENABLE_ANALYZER

namespace ana {

/* Abstract base class for all out-of-bounds warnings.  */

class out_of_bounds : public pending_diagnostic
{
public:
  class oob_region_creation_event_capacity : public region_creation_event_capacity
  {
  public:
    oob_region_creation_event_capacity (tree capacity,
					const event_loc_info &loc_info,
					out_of_bounds &oob)
      : region_creation_event_capacity (capacity,
					loc_info),
	m_oob (oob)
    {
    }
    void prepare_for_emission (checker_path *path,
			       pending_diagnostic *pd,
			       diagnostic_event_id_t emission_id) override
    {
      region_creation_event_capacity::prepare_for_emission (path,
							    pd,
							    emission_id);
      m_oob.m_region_creation_event_id = emission_id;
    }
    private:
    out_of_bounds &m_oob;
  };

  out_of_bounds (const region_model &model,
		 const region *reg,
		 tree diag_arg,
		 const svalue *sval_hint)
  : m_model (model), m_reg (reg), m_diag_arg (diag_arg), m_sval_hint (sval_hint)
  {}

  bool subclass_equal_p (const pending_diagnostic &base_other) const override
  {
    const out_of_bounds &other
      (static_cast <const out_of_bounds &>(base_other));
    return (m_reg == other.m_reg
	    && pending_diagnostic::same_tree_p (m_diag_arg, other.m_diag_arg));
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_out_of_bounds;
  }

  void mark_interesting_stuff (interesting_t *interest) final override
  {
    interest->add_region_creation (m_reg->get_base_region ());
  }

  void add_region_creation_events (const region *,
				   tree capacity,
				   const event_loc_info &loc_info,
				   checker_path &emission_path) override
  {
    /* The memory space is described in the diagnostic message itself,
       so we don't need an event for that.  */
    if (capacity)
      emission_path.add_event
	(make_unique<oob_region_creation_event_capacity> (capacity, loc_info,
							  *this));
  }

  virtual enum access_direction get_dir () const = 0;

protected:
  enum memory_space get_memory_space () const
  {
    return m_reg->get_memory_space ();
  }

  void
  maybe_show_notes (location_t loc, logger *logger) const
  {
    maybe_describe_array_bounds (loc);
    maybe_show_diagram (logger);
  }

  /* Potentially add a note about valid ways to index this array, such
     as (given "int arr[10];"):
       note: valid subscripts for 'arr' are '[0]' to '[9]'
     We print the '[' and ']' characters so as to express the valid
     subscripts using C syntax, rather than just as byte ranges,
     which hopefully is more clear to the user.  */
  void
  maybe_describe_array_bounds (location_t loc) const
  {
    if (!m_diag_arg)
      return;
    tree t = TREE_TYPE (m_diag_arg);
    if (!t)
      return;
    if (TREE_CODE (t) != ARRAY_TYPE)
      return;
    tree domain = TYPE_DOMAIN (t);
    if (!domain)
      return;
    tree max_idx = TYPE_MAX_VALUE (domain);
    if (!max_idx)
      return;
    tree min_idx = TYPE_MIN_VALUE (domain);
    inform (loc,
	    "valid subscripts for %qE are %<[%E]%> to %<[%E]%>",
	    m_diag_arg, min_idx, max_idx);
  }

  void
  maybe_show_diagram (logger *logger) const
  {
    access_operation op (m_model, get_dir (), *m_reg, m_sval_hint);

    /* Don't attempt to make a diagram if there's no valid way of
       accessing the base region (e.g. a 0-element array).  */
    if (op.get_valid_bits ().empty_p ())
      return;

    if (const text_art::theme *theme = global_dc->m_diagrams.m_theme)
      {
	text_art::style_manager sm;
	text_art::canvas canvas (make_access_diagram (op, sm, *theme, logger));
	if (canvas.get_size ().w == 0 && canvas.get_size ().h == 0)
	  {
	    /* In lieu of exceptions, return a zero-sized diagram if there's
	       a problem.  Give up if that's happened.  */
	    return;
	  }
	diagnostic_diagram diagram
	  (canvas,
	   /* Alt text.  */
	   _("Diagram visualizing the predicted out-of-bounds access"));
	diagnostic_emit_diagram (global_dc, diagram);
      }
  }

  text_art::canvas
  make_access_diagram (const access_operation &op,
		       text_art::style_manager &sm,
		       const text_art::theme &theme,
		       logger *logger) const
  {
    access_diagram d (op, m_region_creation_event_id, sm, theme, logger);
    return d.to_canvas (sm);
  }

  region_model m_model;
  const region *m_reg;
  tree m_diag_arg;
  const svalue *m_sval_hint;
  diagnostic_event_id_t m_region_creation_event_id;
};

/* Abstract base class for all out-of-bounds warnings where the
   out-of-bounds range is concrete.  */

class concrete_out_of_bounds : public out_of_bounds
{
public:
  concrete_out_of_bounds (const region_model &model,
			  const region *reg, tree diag_arg,
			  byte_range out_of_bounds_range,
			  const svalue *sval_hint)
  : out_of_bounds (model, reg, diag_arg, sval_hint),
    m_out_of_bounds_range (out_of_bounds_range)
  {}

  bool subclass_equal_p (const pending_diagnostic &base_other) const override
  {
    const concrete_out_of_bounds &other
      (static_cast <const concrete_out_of_bounds &>(base_other));
    return (out_of_bounds::subclass_equal_p (other)
	    && m_out_of_bounds_range == other.m_out_of_bounds_range);
  }

protected:
  byte_range m_out_of_bounds_range;
};

/* Abstract subclass to complaing about concrete out-of-bounds
   past the end of the buffer.  */

class concrete_past_the_end : public concrete_out_of_bounds
{
public:
  concrete_past_the_end (const region_model &model,
			 const region *reg, tree diag_arg, byte_range range,
			 tree byte_bound,
			 const svalue *sval_hint)
  : concrete_out_of_bounds (model, reg, diag_arg, range, sval_hint),
    m_byte_bound (byte_bound)
  {}

  bool
  subclass_equal_p (const pending_diagnostic &base_other) const final override
  {
    const concrete_past_the_end &other
      (static_cast <const concrete_past_the_end &>(base_other));
    return (concrete_out_of_bounds::subclass_equal_p (other)
	    && pending_diagnostic::same_tree_p (m_byte_bound,
						other.m_byte_bound));
  }

  void add_region_creation_events (const region *,
				   tree,
				   const event_loc_info &loc_info,
				   checker_path &emission_path) final override
  {
    if (m_byte_bound && TREE_CODE (m_byte_bound) == INTEGER_CST)
      emission_path.add_event
	(make_unique<oob_region_creation_event_capacity> (m_byte_bound,
							  loc_info,
							  *this));
  }

protected:
  tree m_byte_bound;
};

/* Concrete subclass to complain about buffer overflows.  */

class concrete_buffer_overflow : public concrete_past_the_end
{
public:
  concrete_buffer_overflow (const region_model &model,
			    const region *reg, tree diag_arg,
			    byte_range range, tree byte_bound,
			    const svalue *sval_hint)
  : concrete_past_the_end (model, reg, diag_arg, range, byte_bound, sval_hint)
  {}

  const char *get_kind () const final override
  {
    return "concrete_buffer_overflow";
  }

  bool emit (rich_location *rich_loc,
	     logger *logger) final override
  {
    diagnostic_metadata m;
    bool warned;
    switch (get_memory_space ())
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
	if (wi::fits_uhwi_p (m_out_of_bounds_range.m_size_in_bytes))
	  {
	    unsigned HOST_WIDE_INT num_bad_bytes
	      = m_out_of_bounds_range.m_size_in_bytes.to_uhwi ();
	    if (m_diag_arg)
	      inform_n (rich_loc->get_loc (),
			num_bad_bytes,
			"write of %wu byte to beyond the end of %qE",
			"write of %wu bytes to beyond the end of %qE",
			num_bad_bytes,
			m_diag_arg);
	    else
	      inform_n (rich_loc->get_loc (),
			num_bad_bytes,
			"write of %wu byte to beyond the end of the region",
			"write of %wu bytes to beyond the end of the region",
			num_bad_bytes);
	  }
	else if (m_diag_arg)
	  inform (rich_loc->get_loc (),
		  "write to beyond the end of %qE",
		  m_diag_arg);

	maybe_show_notes (rich_loc->get_loc (), logger);
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

  enum access_direction get_dir () const final override { return DIR_WRITE; }
};

/* Concrete subclass to complain about buffer over-reads.  */

class concrete_buffer_over_read : public concrete_past_the_end
{
public:
  concrete_buffer_over_read (const region_model &model,
			     const region *reg, tree diag_arg,
			     byte_range range, tree byte_bound)
  : concrete_past_the_end (model, reg, diag_arg, range, byte_bound, NULL)
  {}

  const char *get_kind () const final override
  {
    return "concrete_buffer_over_read";
  }

  bool emit (rich_location *rich_loc, logger *logger) final override
  {
    diagnostic_metadata m;
    bool warned;
    m.add_cwe (126);
    switch (get_memory_space ())
      {
      default:
	warned = warning_meta (rich_loc, m, get_controlling_option (),
			       "buffer over-read");
	break;
      case MEMSPACE_STACK:
	warned = warning_meta (rich_loc, m, get_controlling_option (),
			       "stack-based buffer over-read");
	break;
      case MEMSPACE_HEAP:
	warned = warning_meta (rich_loc, m, get_controlling_option (),
			       "heap-based buffer over-read");
	break;
      }

    if (warned)
      {
	if (wi::fits_uhwi_p (m_out_of_bounds_range.m_size_in_bytes))
	  {
	    unsigned HOST_WIDE_INT num_bad_bytes
	      = m_out_of_bounds_range.m_size_in_bytes.to_uhwi ();
	    if (m_diag_arg)
	      inform_n (rich_loc->get_loc (),
			num_bad_bytes,
			"read of %wu byte from after the end of %qE",
			"read of %wu bytes from after the end of %qE",
			num_bad_bytes,
			m_diag_arg);
	    else
	      inform_n (rich_loc->get_loc (),
			num_bad_bytes,
			"read of %wu byte from after the end of the region",
			"read of %wu bytes from after the end of the region",
			num_bad_bytes);
	  }
	else if (m_diag_arg)
	  inform (rich_loc->get_loc (),
		  "read from after the end of %qE",
		  m_diag_arg);

	maybe_show_notes (rich_loc->get_loc (), logger);
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

  enum access_direction get_dir () const final override { return DIR_READ; }
};

/* Concrete subclass to complain about buffer underwrites.  */

class concrete_buffer_underwrite : public concrete_out_of_bounds
{
public:
  concrete_buffer_underwrite (const region_model &model,
			      const region *reg, tree diag_arg,
			      byte_range range,
			      const svalue *sval_hint)
  : concrete_out_of_bounds (model, reg, diag_arg, range, sval_hint)
  {}

  const char *get_kind () const final override
  {
    return "concrete_buffer_underwrite";
  }

  bool emit (rich_location *rich_loc, logger *logger) final override
  {
    diagnostic_metadata m;
    bool warned;
    m.add_cwe (124);
    switch (get_memory_space ())
      {
      default:
	warned = warning_meta (rich_loc, m, get_controlling_option (),
			       "buffer underwrite");
	break;
      case MEMSPACE_STACK:
	warned = warning_meta (rich_loc, m, get_controlling_option (),
			       "stack-based buffer underwrite");
	break;
      case MEMSPACE_HEAP:
	warned = warning_meta (rich_loc, m, get_controlling_option (),
			       "heap-based buffer underwrite");
	break;
      }
    if (warned)
      maybe_show_notes (rich_loc->get_loc (), logger);
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

  enum access_direction get_dir () const final override { return DIR_WRITE; }
};

/* Concrete subclass to complain about buffer under-reads.  */

class concrete_buffer_under_read : public concrete_out_of_bounds
{
public:
  concrete_buffer_under_read (const region_model &model,
			      const region *reg, tree diag_arg,
			      byte_range range)
  : concrete_out_of_bounds (model, reg, diag_arg, range, NULL)
  {}

  const char *get_kind () const final override
  {
    return "concrete_buffer_under_read";
  }

  bool emit (rich_location *rich_loc, logger *logger) final override
  {
    diagnostic_metadata m;
    bool warned;
    m.add_cwe (127);
    switch (get_memory_space ())
      {
      default:
	warned = warning_meta (rich_loc, m, get_controlling_option (),
			       "buffer under-read");
	break;
      case MEMSPACE_STACK:
	warned = warning_meta (rich_loc, m, get_controlling_option (),
			       "stack-based buffer under-read");
	break;
      case MEMSPACE_HEAP:
	warned = warning_meta (rich_loc, m, get_controlling_option (),
			       "heap-based buffer under-read");
	break;
      }
    if (warned)
      maybe_show_notes (rich_loc->get_loc (), logger);
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

  enum access_direction get_dir () const final override { return DIR_READ; }
};

/* Abstract class to complain about out-of-bounds read/writes where
   the values are symbolic.  */

class symbolic_past_the_end : public out_of_bounds
{
public:
  symbolic_past_the_end (const region_model &model,
			 const region *reg, tree diag_arg, tree offset,
			 tree num_bytes, tree capacity,
			 const svalue *sval_hint)
  : out_of_bounds (model, reg, diag_arg, sval_hint),
    m_offset (offset),
    m_num_bytes (num_bytes),
    m_capacity (capacity)
  {}

  bool
  subclass_equal_p (const pending_diagnostic &base_other) const final override
  {
    const symbolic_past_the_end &other
      (static_cast <const symbolic_past_the_end &>(base_other));
    return (out_of_bounds::subclass_equal_p (other)
	    && pending_diagnostic::same_tree_p (m_offset, other.m_offset)
	    && pending_diagnostic::same_tree_p (m_num_bytes, other.m_num_bytes)
	    && pending_diagnostic::same_tree_p (m_capacity, other.m_capacity));
  }

protected:
  tree m_offset;
  tree m_num_bytes;
  tree m_capacity;
};

/* Concrete subclass to complain about overflows with symbolic values.  */

class symbolic_buffer_overflow : public symbolic_past_the_end
{
public:
  symbolic_buffer_overflow (const region_model &model,
			    const region *reg, tree diag_arg, tree offset,
			    tree num_bytes, tree capacity,
			    const svalue *sval_hint)
  : symbolic_past_the_end (model, reg, diag_arg, offset, num_bytes, capacity,
			   sval_hint)
  {
  }

  const char *get_kind () const final override
  {
    return "symbolic_buffer_overflow";
  }

  bool emit (rich_location *rich_loc, logger *logger) final override
  {
    diagnostic_metadata m;
    bool warned;
    switch (get_memory_space ())
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
	warned =  warning_meta (rich_loc, m, get_controlling_option (),
				"heap-based buffer overflow");
	break;
      }
    if (warned)
      maybe_show_notes (rich_loc->get_loc (), logger);
    return warned;
  }

  label_text
  describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_offset)
      {
	/* Known offset.  */
	if (m_num_bytes)
	  {
	    /* Known offset, known size.  */
	    if (TREE_CODE (m_num_bytes) == INTEGER_CST)
	      {
		/* Known offset, known constant size.  */
		if (pending_diagnostic::same_tree_p (m_num_bytes,
						     integer_one_node))
		  {
		    /* Singular m_num_bytes.  */
		    if (m_diag_arg)
		      return ev.formatted_print
			("write of %E byte at offset %qE exceeds %qE",
			 m_num_bytes, m_offset, m_diag_arg);
		    else
		      return ev.formatted_print
			("write of %E byte at offset %qE exceeds the buffer",
			 m_num_bytes, m_offset);
		  }
		else
		  {
		    /* Plural m_num_bytes.  */
		    if (m_diag_arg)
		      return ev.formatted_print
			("write of %E bytes at offset %qE exceeds %qE",
			 m_num_bytes, m_offset, m_diag_arg);
		    else
		      return ev.formatted_print
			("write of %E bytes at offset %qE exceeds the buffer",
			 m_num_bytes, m_offset);
		  }
	      }
	    else
	      {
		/* Known offset, known symbolic size.  */
		if (m_diag_arg)
		  return ev.formatted_print
		    ("write of %qE bytes at offset %qE exceeds %qE",
		     m_num_bytes, m_offset, m_diag_arg);
		else
		  return ev.formatted_print
		    ("write of %qE bytes at offset %qE exceeds the buffer",
		     m_num_bytes, m_offset);
	      }
	  }
	else
	  {
	    /* Known offset, unknown size.  */
	    if (m_diag_arg)
	      return ev.formatted_print ("write at offset %qE exceeds %qE",
					 m_offset, m_diag_arg);
	    else
	      return ev.formatted_print ("write at offset %qE exceeds the"
					 " buffer", m_offset);
	  }
      }
    /* Unknown offset.  */
    if (m_diag_arg)
      return ev.formatted_print ("out-of-bounds write on %qE",
				 m_diag_arg);
    return ev.formatted_print ("out-of-bounds write");
  }

  enum access_direction get_dir () const final override { return DIR_WRITE; }
};

/* Concrete subclass to complain about over-reads with symbolic values.  */

class symbolic_buffer_over_read : public symbolic_past_the_end
{
public:
  symbolic_buffer_over_read (const region_model &model,
			     const region *reg, tree diag_arg, tree offset,
			     tree num_bytes, tree capacity)
  : symbolic_past_the_end (model, reg, diag_arg, offset, num_bytes, capacity,
			   NULL)
  {
  }

  const char *get_kind () const final override
  {
    return "symbolic_buffer_over_read";
  }

  bool emit (rich_location *rich_loc, logger *logger) final override
  {
    diagnostic_metadata m;
    m.add_cwe (126);
    bool warned;
    switch (get_memory_space ())
      {
      default:
	m.add_cwe (787);
	warned = warning_meta (rich_loc, m, get_controlling_option (),
			       "buffer over-read");
	break;
      case MEMSPACE_STACK:
	m.add_cwe (121);
	warned = warning_meta (rich_loc, m, get_controlling_option (),
			       "stack-based buffer over-read");
	break;
      case MEMSPACE_HEAP:
	m.add_cwe (122);
	warned = warning_meta (rich_loc, m, get_controlling_option (),
			       "heap-based buffer over-read");
	break;
      }
    if (warned)
      maybe_show_notes (rich_loc->get_loc (), logger);
    return warned;
  }

  label_text
  describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_offset)
      {
	/* Known offset.  */
	if (m_num_bytes)
	  {
	    /* Known offset, known size.  */
	    if (TREE_CODE (m_num_bytes) == INTEGER_CST)
	      {
		/* Known offset, known constant size.  */
		if (pending_diagnostic::same_tree_p (m_num_bytes,
						     integer_one_node))
		  {
		    /* Singular m_num_bytes.  */
		    if (m_diag_arg)
		      return ev.formatted_print
			("read of %E byte at offset %qE exceeds %qE",
			 m_num_bytes, m_offset, m_diag_arg);
		    else
		      return ev.formatted_print
			("read of %E byte at offset %qE exceeds the buffer",
			 m_num_bytes, m_offset);
		  }
		else
		  {
		    /* Plural m_num_bytes.  */
		    if (m_diag_arg)
		      return ev.formatted_print
			("read of %E bytes at offset %qE exceeds %qE",
			 m_num_bytes, m_offset, m_diag_arg);
		    else
		      return ev.formatted_print
			("read of %E bytes at offset %qE exceeds the buffer",
			 m_num_bytes, m_offset);
		  }
	      }
	    else
	      {
		/* Known offset, known symbolic size.  */
		if (m_diag_arg)
		  return ev.formatted_print
		    ("read of %qE bytes at offset %qE exceeds %qE",
		     m_num_bytes, m_offset, m_diag_arg);
		else
		  return ev.formatted_print
		    ("read of %qE bytes at offset %qE exceeds the buffer",
		     m_num_bytes, m_offset);
	      }
	  }
	else
	  {
	    /* Known offset, unknown size.  */
	    if (m_diag_arg)
	      return ev.formatted_print ("read at offset %qE exceeds %qE",
					 m_offset, m_diag_arg);
	    else
	      return ev.formatted_print ("read at offset %qE exceeds the"
					 " buffer", m_offset);
	  }
      }
    /* Unknown offset.  */
    if (m_diag_arg)
      return ev.formatted_print ("out-of-bounds read on %qE",
				 m_diag_arg);
    return ev.formatted_print ("out-of-bounds read");
  }

  enum access_direction get_dir () const final override { return DIR_READ; }
};

/* Check whether an access is past the end of the BASE_REG.
  Return TRUE if the access was valid, FALSE otherwise.  */

bool
region_model::check_symbolic_bounds (const region *base_reg,
				     const svalue *sym_byte_offset,
				     const svalue *num_bytes_sval,
				     const svalue *capacity,
				     enum access_direction dir,
				     const svalue *sval_hint,
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
      const region *offset_reg = m_mgr->get_offset_region (base_reg,
							   NULL_TREE,
							   sym_byte_offset);
      const region *sized_offset_reg = m_mgr->get_sized_region (offset_reg,
								NULL_TREE,
								num_bytes_sval);
      switch (dir)
	{
	default:
	  gcc_unreachable ();
	  break;
	case DIR_READ:
	  gcc_assert (sval_hint == nullptr);
	  ctxt->warn (make_unique<symbolic_buffer_over_read> (*this,
							      sized_offset_reg,
							      diag_arg,
							      offset_tree,
							      num_bytes_tree,
							      capacity_tree));
	  return false;
	  break;
	case DIR_WRITE:
	  ctxt->warn (make_unique<symbolic_buffer_overflow> (*this,
							     sized_offset_reg,
							     diag_arg,
							     offset_tree,
							     num_bytes_tree,
							     capacity_tree,
							     sval_hint));
	  return false;
	  break;
	}
    }
  return true;
}

static tree
maybe_get_integer_cst_tree (const svalue *sval)
{
  tree cst_tree = sval->maybe_get_constant ();
  if (cst_tree && TREE_CODE (cst_tree) == INTEGER_CST)
    return cst_tree;

  return NULL_TREE;
}

/* May complain when the access on REG is out-of-bounds.
   Return TRUE if the access was valid, FALSE otherwise.  */

bool
region_model::check_region_bounds (const region *reg,
				   enum access_direction dir,
				   const svalue *sval_hint,
				   region_model_context *ctxt) const
{
  gcc_assert (ctxt);

  /* Get the offset.  */
  region_offset reg_offset = reg->get_offset (m_mgr);
  const region *base_reg = reg_offset.get_base_region ();

  /* Find out how many bytes were accessed.  */
  const svalue *num_bytes_sval = reg->get_byte_size_sval (m_mgr);
  tree num_bytes_tree = maybe_get_integer_cst_tree (num_bytes_sval);
  /* Bail out if 0 bytes are accessed.  */
  if (num_bytes_tree && zerop (num_bytes_tree))
	  return true;

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

  /* If any of the base region, the offset, or the number of bytes accessed
     are symbolic, we have to reason about symbolic values.  */
  if (base_reg->symbolic_p () || reg_offset.symbolic_p () || !num_bytes_tree)
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
      return check_symbolic_bounds (base_reg, byte_offset_sval, num_bytes_sval,
			     capacity, dir, sval_hint, ctxt);
    }

  /* Otherwise continue to check with concrete values.  */
  byte_range out (0, 0);
  bool oob_safe = true;
  /* NUM_BYTES_TREE should always be interpreted as unsigned.  */
  byte_offset_t num_bytes_unsigned = wi::to_offset (num_bytes_tree);
  byte_range read_bytes (offset, num_bytes_unsigned);
  /* If read_bytes has a subset < 0, we do have an underwrite.  */
  if (read_bytes.falls_short_of_p (0, &out))
    {
      tree diag_arg = get_representative_tree (base_reg);
      switch (dir)
	{
	default:
	  gcc_unreachable ();
	  break;
	case DIR_READ:
	  gcc_assert (sval_hint == nullptr);
	  ctxt->warn (make_unique<concrete_buffer_under_read> (*this, reg,
							       diag_arg,
							       out));
	  oob_safe = false;
	  break;
	case DIR_WRITE:
	  ctxt->warn (make_unique<concrete_buffer_underwrite> (*this,
							       reg, diag_arg,
							       out,
							       sval_hint));
	  oob_safe = false;
	  break;
	}
    }

  /* For accesses past the end, we do need a concrete capacity.  No need to
     do a symbolic check here because the inequality check does not reason
     whether constants are greater than symbolic values.  */
  if (!cst_capacity_tree)
	  return oob_safe;

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
	  gcc_assert (sval_hint == nullptr);
	  ctxt->warn (make_unique<concrete_buffer_over_read> (*this,
							      reg, diag_arg,
							      out, byte_bound));
	  oob_safe = false;
	  break;
	case DIR_WRITE:
	  ctxt->warn (make_unique<concrete_buffer_overflow> (*this,
							     reg, diag_arg,
							     out, byte_bound,
							     sval_hint));
	  oob_safe = false;
	  break;
	}
    }
  return oob_safe;
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
