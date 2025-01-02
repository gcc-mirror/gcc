/* Bounds-checking of reads and writes to memory regions.
   Copyright (C) 2019-2025 Free Software Foundation, Inc.

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
#include "diagnostic-diagram.h"
#include "diagnostic-format-sarif.h"
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
    oob_region_creation_event_capacity (tree byte_capacity,
					const event_loc_info &loc_info,
					out_of_bounds &oob)
    : region_creation_event_capacity (byte_capacity,
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
				   tree byte_capacity,
				   const event_loc_info &loc_info,
				   checker_path &emission_path) override
  {
    /* The memory space is described in the diagnostic message itself,
       so we don't need an event for that.  */
    if (byte_capacity)
      emission_path.add_event
	(make_unique<oob_region_creation_event_capacity> (byte_capacity,
							  loc_info,
							  *this));
  }

  void maybe_add_sarif_properties (sarif_object &result_obj)
    const override
  {
    sarif_property_bag &props = result_obj.get_or_create_properties ();
#define PROPERTY_PREFIX "gcc/analyzer/out_of_bounds/"
    props.set_string (PROPERTY_PREFIX "dir",
		      get_dir () == DIR_READ ? "read" : "write");
    props.set (PROPERTY_PREFIX "model", m_model.to_json ());
    props.set (PROPERTY_PREFIX "region", m_reg->to_json ());
    props.set (PROPERTY_PREFIX "diag_arg", tree_to_json (m_diag_arg));
    if (m_sval_hint)
      props.set (PROPERTY_PREFIX "sval_hint", m_sval_hint->to_json ());
    props.set (PROPERTY_PREFIX "region_creation_event_id",
	       diagnostic_event_id_to_json (m_region_creation_event_id));
#undef PROPERTY_PREFIX
  }

  virtual enum access_direction get_dir () const = 0;

protected:
  enum memory_space get_memory_space () const
  {
    return m_reg->get_memory_space ();
  }

  void
  maybe_show_notes (diagnostic_emission_context &ctxt) const
  {
    maybe_describe_array_bounds (ctxt.get_location ());
    maybe_show_diagram (ctxt.get_logger ());
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

    if (const text_art::theme *theme = global_dc->get_diagram_theme ())
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
	global_dc->emit_diagram (diagram);
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
			  bit_range out_of_bounds_bits,
			  const svalue *sval_hint)
  : out_of_bounds (model, reg, diag_arg, sval_hint),
    m_out_of_bounds_bits (out_of_bounds_bits)
  {}

  bool subclass_equal_p (const pending_diagnostic &base_other) const override
  {
    const concrete_out_of_bounds &other
      (static_cast <const concrete_out_of_bounds &>(base_other));
    return (out_of_bounds::subclass_equal_p (other)
	    && m_out_of_bounds_bits == other.m_out_of_bounds_bits);
  }

  void maybe_add_sarif_properties (sarif_object &result_obj)
    const override
  {
    out_of_bounds::maybe_add_sarif_properties (result_obj);
    sarif_property_bag &props = result_obj.get_or_create_properties ();
#define PROPERTY_PREFIX "gcc/analyzer/concrete_out_of_bounds/"
    props.set (PROPERTY_PREFIX "out_of_bounds_bits",
	       m_out_of_bounds_bits.to_json ());
    byte_range out_of_bounds_bytes (0, 0);
    if (get_out_of_bounds_bytes (&out_of_bounds_bytes))
      props.set (PROPERTY_PREFIX "out_of_bounds_bytes",
		 out_of_bounds_bytes.to_json ());
#undef PROPERTY_PREFIX
  }

  bool get_out_of_bounds_bytes (byte_range *out) const
  {
    return m_out_of_bounds_bits.as_byte_range (out);
  }

protected:
  bit_range m_out_of_bounds_bits;
};

/* Abstract subclass to complaing about concrete out-of-bounds
   past the end of the buffer.  */

class concrete_past_the_end : public concrete_out_of_bounds
{
public:
  concrete_past_the_end (const region_model &model,
			 const region *reg, tree diag_arg, bit_range range,
			 tree bit_bound,
			 const svalue *sval_hint)
  : concrete_out_of_bounds (model, reg, diag_arg, range, sval_hint),
    m_bit_bound (bit_bound),
    m_byte_bound (NULL_TREE)
  {
    if (m_bit_bound && TREE_CODE (m_bit_bound) == INTEGER_CST)
      m_byte_bound
	= wide_int_to_tree (size_type_node,
			    wi::to_offset (m_bit_bound) >> LOG2_BITS_PER_UNIT);
  }

  bool
  subclass_equal_p (const pending_diagnostic &base_other) const final override
  {
    const concrete_past_the_end &other
      (static_cast <const concrete_past_the_end &>(base_other));
    return (concrete_out_of_bounds::subclass_equal_p (other)
	    && pending_diagnostic::same_tree_p (m_bit_bound,
						other.m_bit_bound));
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

  void maybe_add_sarif_properties (sarif_object &result_obj)
    const final override
  {
    concrete_out_of_bounds::maybe_add_sarif_properties (result_obj);
    sarif_property_bag &props = result_obj.get_or_create_properties ();
#define PROPERTY_PREFIX "gcc/analyzer/concrete_past_the_end/"
    props.set (PROPERTY_PREFIX "bit_bound",
	       tree_to_json (m_bit_bound));
    props.set (PROPERTY_PREFIX "byte_bound",
	       tree_to_json (m_byte_bound));
#undef PROPERTY_PREFIX
  }

protected:
  tree m_bit_bound;
  tree m_byte_bound;
};

/* Concrete subclass to complain about buffer overflows.  */

class concrete_buffer_overflow : public concrete_past_the_end
{
public:
  concrete_buffer_overflow (const region_model &model,
			    const region *reg, tree diag_arg,
			    bit_range range, tree bit_bound,
			    const svalue *sval_hint)
  : concrete_past_the_end (model, reg, diag_arg, range, bit_bound, sval_hint)
  {}

  const char *get_kind () const final override
  {
    return "concrete_buffer_overflow";
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    bool warned;
    switch (get_memory_space ())
      {
      default:
	ctxt.add_cwe (787);
	warned = ctxt.warn ("buffer overflow");
	break;
      case MEMSPACE_STACK:
	ctxt.add_cwe (121);
	warned = ctxt.warn ("stack-based buffer overflow");
	break;
      case MEMSPACE_HEAP:
	ctxt.add_cwe (122);
	warned = ctxt.warn ("heap-based buffer overflow");
	break;
      }

    if (warned)
      {
	if (wi::fits_uhwi_p (m_out_of_bounds_bits.m_size_in_bits))
	  {
	    unsigned HOST_WIDE_INT num_bad_bits
	      = m_out_of_bounds_bits.m_size_in_bits.to_uhwi ();
	    if (num_bad_bits % BITS_PER_UNIT == 0)
	      {
		unsigned HOST_WIDE_INT num_bad_bytes
		  = num_bad_bits / BITS_PER_UNIT;
		if (m_diag_arg)
		  inform_n (ctxt.get_location (),
			    num_bad_bytes,
			    "write of %wu byte to beyond the end of %qE",
			    "write of %wu bytes to beyond the end of %qE",
			    num_bad_bytes,
			    m_diag_arg);
		else
		  inform_n (ctxt.get_location (),
			    num_bad_bytes,
			    "write of %wu byte to beyond the end of the region",
			    "write of %wu bytes to beyond the end of the region",
			    num_bad_bytes);
	      }
	    else
	      {
		if (m_diag_arg)
		  inform_n (ctxt.get_location (),
			    num_bad_bits,
			    "write of %wu bit to beyond the end of %qE",
			    "write of %wu bits to beyond the end of %qE",
			    num_bad_bits,
			    m_diag_arg);
		else
		  inform_n (ctxt.get_location (),
			    num_bad_bits,
			    "write of %wu bit to beyond the end of the region",
			    "write of %wu bits to beyond the end of the region",
			    num_bad_bits);
	      }
	  }
	else if (m_diag_arg)
	  inform (ctxt.get_location (),
		  "write to beyond the end of %qE",
		  m_diag_arg);

	maybe_show_notes (ctxt);
      }

    return warned;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    if (m_byte_bound || !m_bit_bound)
      {
	byte_range out_of_bounds_bytes (0, 0);
	if (get_out_of_bounds_bytes (&out_of_bounds_bytes))
	  {
	    describe_final_event_as_bytes (pp, out_of_bounds_bytes);
	    return true;
	  }
      }
    describe_final_event_as_bits (pp);
    return true;
  }

  void
  describe_final_event_as_bytes (pretty_printer &pp,
				 const byte_range &out_of_bounds_bytes)
  {
    byte_size_t start = out_of_bounds_bytes.get_start_byte_offset ();
    byte_size_t end = out_of_bounds_bytes.get_last_byte_offset ();
    char start_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (start, start_buf, SIGNED);
    char end_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (end, end_buf, SIGNED);

    if (start == end)
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write at byte %s but %qE"
		     " ends at byte %E", start_buf, m_diag_arg,
		     m_byte_bound);
	else
	  pp_printf (&pp,
		     "out-of-bounds write at byte %s but region"
		     " ends at byte %E", start_buf,
		     m_byte_bound);
      }
    else
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write from byte %s till"
		     " byte %s but %qE ends at byte %E",
		     start_buf, end_buf, m_diag_arg,
		     m_byte_bound);
	else
	  pp_printf (&pp,
		     "out-of-bounds write from byte %s till"
		     " byte %s but region ends at byte %E",
		     start_buf, end_buf, m_byte_bound);
      }
  }

  void
  describe_final_event_as_bits (pretty_printer &pp)
  {
    bit_size_t start = m_out_of_bounds_bits.get_start_bit_offset ();
    bit_size_t end = m_out_of_bounds_bits.get_last_bit_offset ();
    char start_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (start, start_buf, SIGNED);
    char end_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (end, end_buf, SIGNED);

    if (start == end)
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write at bit %s but %qE"
		     " ends at bit %E", start_buf, m_diag_arg,
		     m_bit_bound);
	else
	  pp_printf (&pp,
		     "out-of-bounds write at bit %s but region"
		     " ends at bit %E", start_buf,
		     m_bit_bound);
      }
    else
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write from bit %s till"
		     " bit %s but %qE ends at bit %E",
		     start_buf, end_buf, m_diag_arg,
		     m_bit_bound);
	else
	  pp_printf (&pp,
		     "out-of-bounds write from bit %s till"
		     " bit %s but region ends at bit %E",
		     start_buf, end_buf, m_bit_bound);
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
			     bit_range range, tree bit_bound)
  : concrete_past_the_end (model, reg, diag_arg, range, bit_bound, NULL)
  {}

  const char *get_kind () const final override
  {
    return "concrete_buffer_over_read";
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    bool warned;
    ctxt.add_cwe (126);
    switch (get_memory_space ())
      {
      default:
	warned = ctxt.warn ("buffer over-read");
	break;
      case MEMSPACE_STACK:
	warned = ctxt.warn ("stack-based buffer over-read");
	break;
      case MEMSPACE_HEAP:
	warned = ctxt.warn ("heap-based buffer over-read");
	break;
      }

    if (warned)
      {
	if (wi::fits_uhwi_p (m_out_of_bounds_bits.m_size_in_bits))
	  {
	    unsigned HOST_WIDE_INT num_bad_bits
	      = m_out_of_bounds_bits.m_size_in_bits.to_uhwi ();
	    if (num_bad_bits % BITS_PER_UNIT == 0)
	      {
		unsigned HOST_WIDE_INT num_bad_bytes
		  = num_bad_bits / BITS_PER_UNIT;
		if (m_diag_arg)
		  inform_n (ctxt.get_location (),
			    num_bad_bytes,
			    "read of %wu byte from after the end of %qE",
			    "read of %wu bytes from after the end of %qE",
			    num_bad_bytes,
			    m_diag_arg);
		else
		  inform_n (ctxt.get_location (),
			    num_bad_bytes,
			    "read of %wu byte from after the end of the region",
			    "read of %wu bytes from after the end of the region",
			    num_bad_bytes);
	      }
	    else
	      {
		if (m_diag_arg)
		  inform_n (ctxt.get_location (),
			    num_bad_bits,
			    "read of %wu bit from after the end of %qE",
			    "read of %wu bits from after the end of %qE",
			    num_bad_bits,
			    m_diag_arg);
		else
		  inform_n (ctxt.get_location (),
			    num_bad_bits,
			    "read of %wu bit from after the end of the region",
			    "read of %wu bits from after the end of the region",
			    num_bad_bits);
	      }
	  }
	else if (m_diag_arg)
	  inform (ctxt.get_location (),
		  "read from after the end of %qE",
		  m_diag_arg);

	maybe_show_notes (ctxt);
      }

    return warned;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    if (m_byte_bound || !m_bit_bound)
      {
	byte_range out_of_bounds_bytes (0, 0);
	if (get_out_of_bounds_bytes (&out_of_bounds_bytes))
	  {
	    describe_final_event_as_bytes (pp, out_of_bounds_bytes);
	    return true;
	  }
      }
    describe_final_event_as_bits (pp);
    return true;
  }

  void
  describe_final_event_as_bytes (pretty_printer &pp,
				 const byte_range &out_of_bounds_bytes)
  {
    byte_size_t start = out_of_bounds_bytes.get_start_byte_offset ();
    byte_size_t end = out_of_bounds_bytes.get_last_byte_offset ();
    char start_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (start, start_buf, SIGNED);
    char end_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (end, end_buf, SIGNED);

    if (start == end)
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read at byte %s but %qE"
		     " ends at byte %E", start_buf, m_diag_arg,
		     m_byte_bound);
	else
	  pp_printf (&pp,
		     "out-of-bounds read at byte %s but region"
		     " ends at byte %E", start_buf,
		     m_byte_bound);
      }
    else
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read from byte %s till"
		     " byte %s but %qE ends at byte %E",
		     start_buf, end_buf, m_diag_arg,
		     m_byte_bound);
	else
	  pp_printf (&pp,
		     "out-of-bounds read from byte %s till"
		     " byte %s but region ends at byte %E",
		     start_buf, end_buf, m_byte_bound);
      }
  }

  void
  describe_final_event_as_bits (pretty_printer &pp)
  {
    bit_size_t start = m_out_of_bounds_bits.get_start_bit_offset ();
    bit_size_t end = m_out_of_bounds_bits.get_last_bit_offset ();
    char start_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (start, start_buf, SIGNED);
    char end_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (end, end_buf, SIGNED);

    if (start == end)
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read at bit %s but %qE"
		     " ends at bit %E", start_buf, m_diag_arg,
		     m_bit_bound);
	else
	  pp_printf (&pp,
		     "out-of-bounds read at bit %s but region"
		     " ends at bit %E", start_buf,
		     m_bit_bound);
      }
    else
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read from bit %s till"
		     " bit %s but %qE ends at bit %E",
		     start_buf, end_buf, m_diag_arg,
		     m_bit_bound);
	else
	  pp_printf (&pp,
		     "out-of-bounds read from bit %s till"
		     " bit %s but region ends at bit %E",
		     start_buf, end_buf, m_bit_bound);
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
			      bit_range range,
			      const svalue *sval_hint)
  : concrete_out_of_bounds (model, reg, diag_arg, range, sval_hint)
  {}

  const char *get_kind () const final override
  {
    return "concrete_buffer_underwrite";
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    bool warned;
    ctxt.add_cwe (124);
    switch (get_memory_space ())
      {
      default:
	warned = ctxt.warn ("buffer underwrite");
	break;
      case MEMSPACE_STACK:
	warned = ctxt.warn ("stack-based buffer underwrite");
	break;
      case MEMSPACE_HEAP:
	warned = ctxt.warn ("heap-based buffer underwrite");
	break;
      }
    if (warned)
      maybe_show_notes (ctxt);
    return warned;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    byte_range out_of_bounds_bytes (0, 0);
    if (get_out_of_bounds_bytes (&out_of_bounds_bytes))
      describe_final_event_as_bytes (pp, out_of_bounds_bytes);
    else
      describe_final_event_as_bits (pp);
    return true;
  }

  void
  describe_final_event_as_bytes (pretty_printer &pp,
				 const byte_range &out_of_bounds_bytes)
  {
    byte_size_t start = out_of_bounds_bytes.get_start_byte_offset ();
    byte_size_t end = out_of_bounds_bytes.get_last_byte_offset ();
    char start_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (start, start_buf, SIGNED);
    char end_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (end, end_buf, SIGNED);

    if (start == end)
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write at byte %s but %qE"
		     " starts at byte 0",
		     start_buf, m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds write at byte %s but region"
		     " starts at byte 0", start_buf);
      }
    else
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write from byte %s till"
		     " byte %s but %qE starts at byte 0",
		     start_buf, end_buf, m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds write from byte %s till"
		     " byte %s but region starts at byte 0",
		     start_buf, end_buf);;
      }
  }

  void
  describe_final_event_as_bits (pretty_printer &pp)
  {
    bit_size_t start = m_out_of_bounds_bits.get_start_bit_offset ();
    bit_size_t end = m_out_of_bounds_bits.get_last_bit_offset ();
    char start_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (start, start_buf, SIGNED);
    char end_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (end, end_buf, SIGNED);

    if (start == end)
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write at bit %s but %qE"
		     " starts at bit 0",
		     start_buf, m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds write at bit %s but region"
		     " starts at bit 0", start_buf);
      }
    else
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write from bit %s till"
		     " bit %s but %qE starts at bit 0",
		     start_buf, end_buf, m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds write from bit %s till"
		     " bit %s but region starts at bit 0",
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
			      bit_range range)
  : concrete_out_of_bounds (model, reg, diag_arg, range, NULL)
  {}

  const char *get_kind () const final override
  {
    return "concrete_buffer_under_read";
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    bool warned;
    ctxt.add_cwe (127);
    switch (get_memory_space ())
      {
      default:
	warned = ctxt.warn ("buffer under-read");
	break;
      case MEMSPACE_STACK:
	warned = ctxt.warn ("stack-based buffer under-read");
	break;
      case MEMSPACE_HEAP:
	warned = ctxt.warn ("heap-based buffer under-read");
	break;
      }
    if (warned)
      maybe_show_notes (ctxt);
    return warned;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    byte_range out_of_bounds_bytes (0, 0);
    if (get_out_of_bounds_bytes (&out_of_bounds_bytes))
      describe_final_event_as_bytes (pp, out_of_bounds_bytes);
    else
      describe_final_event_as_bits (pp);
    return true;
  }

  void
  describe_final_event_as_bytes (pretty_printer &pp,
				 const byte_range &out_of_bounds_bytes)
  {
    byte_size_t start = out_of_bounds_bytes.get_start_byte_offset ();
    byte_size_t end = out_of_bounds_bytes.get_last_byte_offset ();
    char start_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (start, start_buf, SIGNED);
    char end_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (end, end_buf, SIGNED);

    if (start == end)
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read at byte %s but %qE"
		     " starts at byte 0",
		     start_buf, m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds read at byte %s but region"
		     " starts at byte 0",
		     start_buf);
      }
    else
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read from byte %s till"
		     " byte %s but %qE starts at byte 0",
		     start_buf, end_buf, m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds read from byte %s till"
		     " byte %s but region starts at byte 0",
		     start_buf, end_buf);;
      }
  }

  void
  describe_final_event_as_bits (pretty_printer &pp)
  {
    bit_size_t start = m_out_of_bounds_bits.get_start_bit_offset ();
    bit_size_t end = m_out_of_bounds_bits.get_last_bit_offset ();
    char start_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (start, start_buf, SIGNED);
    char end_buf[WIDE_INT_PRINT_BUFFER_SIZE];
    print_dec (end, end_buf, SIGNED);

    if (start == end)
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read at bit %s but %qE"
		     " starts at bit 0",
		     start_buf, m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds read at bit %s but region"
		     " starts at bit 0", start_buf);
      }
    else
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read from bit %s till"
		     " bit %s but %qE starts at bit 0",
		     start_buf, end_buf, m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds read from bit %s till"
		     " bit %s but region starts at bit 0",
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

  void maybe_add_sarif_properties (sarif_object &result_obj)
    const final override
  {
    out_of_bounds::maybe_add_sarif_properties (result_obj);
    sarif_property_bag &props = result_obj.get_or_create_properties ();
#define PROPERTY_PREFIX "gcc/analyzer/symbolic_past_the_end/"
    props.set (PROPERTY_PREFIX "offset", tree_to_json (m_offset));
    props.set (PROPERTY_PREFIX "num_bytes", tree_to_json (m_num_bytes));
    props.set (PROPERTY_PREFIX "capacity", tree_to_json (m_capacity));
#undef PROPERTY_PREFIX
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

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    bool warned;
    switch (get_memory_space ())
      {
      default:
	ctxt.add_cwe (787);
	warned = ctxt.warn ("buffer overflow");
	break;
      case MEMSPACE_STACK:
	ctxt.add_cwe (121);
	warned = ctxt.warn ("stack-based buffer overflow");
	break;
      case MEMSPACE_HEAP:
	ctxt.add_cwe (122);
	warned =  ctxt.warn ("heap-based buffer overflow");
	break;
      }
    if (warned)
      maybe_show_notes (ctxt);
    return warned;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
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
		      pp_printf (&pp,
				 "write of %E byte at offset %qE exceeds %qE",
				 m_num_bytes, m_offset, m_diag_arg);
		    else
		      pp_printf (&pp,
				 "write of %E byte at offset %qE exceeds"
				 " the buffer",
				 m_num_bytes, m_offset);
		  }
		else
		  {
		    /* Plural m_num_bytes.  */
		    if (m_diag_arg)
		      pp_printf (&pp,
				 "write of %E bytes at offset %qE exceeds %qE",
				 m_num_bytes, m_offset, m_diag_arg);
		    else
		      pp_printf (&pp,
				 "write of %E bytes at offset %qE exceeds"
				 " the buffer",
				 m_num_bytes, m_offset);
		  }
	      }
	    else
	      {
		/* Known offset, known symbolic size.  */
		if (m_diag_arg)
		  pp_printf (&pp,
			     "write of %qE bytes at offset %qE exceeds %qE",
			     m_num_bytes, m_offset, m_diag_arg);
		else
		  pp_printf (&pp,
			     "write of %qE bytes at offset %qE exceeds"
			     " the buffer",
			     m_num_bytes, m_offset);
	      }
	  }
	else
	  {
	    /* Known offset, unknown size.  */
	    if (m_diag_arg)
	      pp_printf (&pp,
			 "write at offset %qE exceeds %qE",
			 m_offset, m_diag_arg);
	    else
	      pp_printf (&pp,
			 "write at offset %qE exceeds the buffer",
			 m_offset);
	  }
      }
    else
      {
	/* Unknown offset.  */
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write on %qE",
		     m_diag_arg);
	else
	  pp_printf (&pp, "out-of-bounds write");
      }
    return true;
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

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    ctxt.add_cwe (126);
    bool warned;
    switch (get_memory_space ())
      {
      default:
	ctxt.add_cwe (787);
	warned = ctxt.warn ("buffer over-read");
	break;
      case MEMSPACE_STACK:
	ctxt.add_cwe (121);
	warned = ctxt.warn ("stack-based buffer over-read");
	break;
      case MEMSPACE_HEAP:
	ctxt.add_cwe (122);
	warned = ctxt.warn ("heap-based buffer over-read");
	break;
      }
    if (warned)
      maybe_show_notes (ctxt);
    return warned;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
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
		      pp_printf (&pp,
				 "read of %E byte at offset %qE exceeds %qE",
				 m_num_bytes, m_offset, m_diag_arg);
		    else
		      pp_printf (&pp,
				 "read of %E byte at offset %qE exceeds"
				 " the buffer",
				 m_num_bytes, m_offset);
		  }
		else
		  {
		    /* Plural m_num_bytes.  */
		    if (m_diag_arg)
		      pp_printf (&pp,
				 "read of %E bytes at offset %qE exceeds %qE",
				 m_num_bytes, m_offset, m_diag_arg);
		    else
		      pp_printf (&pp,
				 "read of %E bytes at offset %qE exceeds"
				 " the buffer",
				 m_num_bytes, m_offset);
		  }
	      }
	    else
	      {
		/* Known offset, known symbolic size.  */
		if (m_diag_arg)
		  pp_printf (&pp,
			     "read of %qE bytes at offset %qE exceeds %qE",
			     m_num_bytes, m_offset, m_diag_arg);
		else
		  pp_printf (&pp,
			     "read of %qE bytes at offset %qE exceeds"
			     " the buffer",
			     m_num_bytes, m_offset);
	      }
	  }
	else
	  {
	    /* Known offset, unknown size.  */
	    if (m_diag_arg)
	      pp_printf (&pp,
			 "read at offset %qE exceeds %qE",
			 m_offset, m_diag_arg);
	    else
	      pp_printf (&pp,
			 "read at offset %qE exceeds the buffer",
			 m_offset);
	  }
      }
    else
      {
	/* Unknown offset.  */
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read on %qE",
		     m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds read");
      }
    return true;
  }

  enum access_direction get_dir () const final override { return DIR_READ; }
};

const svalue *
strip_types (const svalue *sval,
	     region_model_manager &mgr)
{
  switch (sval->get_kind ())
    {
    default:
      gcc_unreachable ();
    case SK_REGION:
      {
	const region_svalue *region_sval = (const region_svalue *)sval;
	return mgr.get_ptr_svalue (NULL_TREE, region_sval->get_pointee ());
      }
    case SK_CONSTANT:
      return sval;
    case SK_UNKNOWN:
      return mgr.get_or_create_unknown_svalue (NULL_TREE);
    case SK_POISONED:
      {
	const poisoned_svalue *poisoned_sval = (const poisoned_svalue *)sval;
	return mgr.get_or_create_poisoned_svalue
	  (poisoned_sval->get_poison_kind (),
	   NULL_TREE);
      }
    case SK_SETJMP:
      return sval;
    case SK_INITIAL:
      return sval;
    case SK_UNARYOP:
      {
	const unaryop_svalue *unaryop_sval = (const unaryop_svalue *)sval;
	const enum tree_code op = unaryop_sval->get_op ();
	if (op == VIEW_CONVERT_EXPR || op == NOP_EXPR)
	  return strip_types (unaryop_sval->get_arg (), mgr);
	return mgr.get_or_create_unaryop
	  (NULL_TREE,
	   op,
	   strip_types (unaryop_sval->get_arg (), mgr));
      }
    case SK_BINOP:
      {
	const binop_svalue *binop_sval = (const binop_svalue *)sval;
	const enum tree_code op = binop_sval->get_op ();
	return mgr.get_or_create_binop
	  (NULL_TREE,
	   op,
	   strip_types (binop_sval->get_arg0 (), mgr),
	   strip_types (binop_sval->get_arg1 (), mgr));
      }
    case SK_SUB:
      {
	const sub_svalue *sub_sval = (const sub_svalue *)sval;
	return mgr.get_or_create_sub_svalue
	  (NULL_TREE,
	   strip_types (sub_sval->get_parent (), mgr),
	   sub_sval->get_subregion ());
      }
    case SK_REPEATED:
      {
	const repeated_svalue *repeated_sval = (const repeated_svalue *)sval;
	return mgr.get_or_create_repeated_svalue
	  (NULL_TREE,
	   strip_types (repeated_sval->get_outer_size (), mgr),
	   strip_types (repeated_sval->get_inner_svalue (), mgr));
      }
    case SK_BITS_WITHIN:
      {
	const bits_within_svalue *bits_within_sval
	  = (const bits_within_svalue *)sval;
	return mgr.get_or_create_bits_within
	  (NULL_TREE,
	   bits_within_sval->get_bits (),
	   strip_types (bits_within_sval->get_inner_svalue (), mgr));
      }
    case SK_UNMERGEABLE:
      {
	const unmergeable_svalue *unmergeable_sval
	  = (const unmergeable_svalue *)sval;
	return mgr.get_or_create_unmergeable
	  (strip_types (unmergeable_sval->get_arg (), mgr));
      }
    case SK_PLACEHOLDER:
      return sval;
    case SK_WIDENING:
      {
	const widening_svalue *widening_sval = (const widening_svalue *)sval;
	return mgr.get_or_create_widening_svalue
	  (NULL_TREE,
	   widening_sval->get_point (),
	   strip_types (widening_sval->get_base_svalue (), mgr),
	   strip_types (widening_sval->get_iter_svalue (), mgr));
      }
    case SK_COMPOUND:
      {
	const compound_svalue *compound_sval = (const compound_svalue *)sval;
	binding_map typeless_map;
	for (auto iter : compound_sval->get_map ())
	  {
	    const binding_key *key = iter.first;
	    const svalue *bound_sval = iter.second;
	    typeless_map.put (key, strip_types (bound_sval, mgr));
	  }
	return mgr.get_or_create_compound_svalue (NULL_TREE, typeless_map);
      }
    case SK_CONJURED:
      return sval;
    case SK_ASM_OUTPUT:
      {
	const asm_output_svalue *asm_output_sval
	  = (const asm_output_svalue *)sval;
	auto_vec<const svalue *> typeless_inputs
	  (asm_output_sval->get_num_inputs ());
	for (unsigned idx = 0; idx < asm_output_sval->get_num_inputs (); idx++)
	  typeless_inputs.quick_push
	    (strip_types (asm_output_sval->get_input (idx),
			  mgr));
	return mgr.get_or_create_asm_output_svalue
	  (NULL_TREE,
	   asm_output_sval->get_asm_string (),
	   asm_output_sval->get_output_idx (),
	   asm_output_sval->get_num_outputs (),
	   typeless_inputs);
      }
    case SK_CONST_FN_RESULT:
      {
	const const_fn_result_svalue *const_fn_result_sval
	  = (const const_fn_result_svalue *)sval;
	auto_vec<const svalue *> typeless_inputs
	  (const_fn_result_sval->get_num_inputs ());
	for (unsigned idx = 0;
	     idx < const_fn_result_sval->get_num_inputs ();
	     idx++)
	  typeless_inputs.quick_push
	    (strip_types (const_fn_result_sval->get_input (idx),
			  mgr));
	return mgr.get_or_create_const_fn_result_svalue
	  (NULL_TREE,
	   const_fn_result_sval->get_fndecl (),
	   typeless_inputs);
      }
    }
}

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
    = m_mgr->get_or_create_binop (NULL_TREE, PLUS_EXPR,
				  sym_byte_offset, num_bytes_sval);

  next_byte = strip_types (next_byte, *m_mgr);
  capacity = strip_types (capacity, *m_mgr);

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

  /* Find out how many bits were accessed.  */
  const svalue *num_bits_sval = reg->get_bit_size_sval (m_mgr);
  tree num_bits_tree = maybe_get_integer_cst_tree (num_bits_sval);
  /* Bail out if 0 bits are accessed.  */
  if (num_bits_tree && zerop (num_bits_tree))
	  return true;

  /* Get the capacity of the buffer (in bytes).  */
  const svalue *byte_capacity = get_capacity (base_reg);
  tree cst_byte_capacity_tree = maybe_get_integer_cst_tree (byte_capacity);

  /* The constant offset from a pointer is represented internally as a sizetype
     but should be interpreted as a signed value here.  The statement below
     converts the offset from bits to bytes and then to a signed integer with
     the same precision the sizetype has on the target system.

     For example, this is needed for out-of-bounds-3.c test1 to pass when
     compiled with a 64-bit gcc build targeting 32-bit systems.  */
  bit_offset_t bit_offset;
  if (!reg_offset.symbolic_p ())
    bit_offset = wi::sext (reg_offset.get_bit_offset (),
			   TYPE_PRECISION (size_type_node));

  /* If any of the base region, the offset, or the number of bytes accessed
     are symbolic, we have to reason about symbolic values.  */
  if (base_reg->symbolic_p () || reg_offset.symbolic_p () || !num_bits_tree)
    {
      const svalue* byte_offset_sval;
      if (!reg_offset.symbolic_p ())
	{
	  tree byte_offset_tree
	    = wide_int_to_tree (integer_type_node,
				bit_offset >> LOG2_BITS_PER_UNIT);
	  byte_offset_sval
	    = m_mgr->get_or_create_constant_svalue (byte_offset_tree);
	}
      else
	byte_offset_sval = reg_offset.get_symbolic_byte_offset ();
      const svalue *num_bytes_sval = reg->get_byte_size_sval (m_mgr);
      return check_symbolic_bounds (base_reg, byte_offset_sval, num_bytes_sval,
				    byte_capacity, dir, sval_hint, ctxt);
    }

  /* Otherwise continue to check with concrete values.  */
  bit_range bits_outside (0, 0);
  bool oob_safe = true;
  /* NUM_BITS_TREE should always be interpreted as unsigned.  */
  bit_offset_t num_bits_unsigned = wi::to_offset (num_bits_tree);
  bit_range read_bits (bit_offset, num_bits_unsigned);
  /* If read_bits has a subset < 0, we do have an underwrite.  */
  if (read_bits.falls_short_of_p (0, &bits_outside))
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
							       bits_outside));
	  oob_safe = false;
	  break;
	case DIR_WRITE:
	  ctxt->warn (make_unique<concrete_buffer_underwrite> (*this,
							       reg, diag_arg,
							       bits_outside,
							       sval_hint));
	  oob_safe = false;
	  break;
	}
    }

  /* For accesses past the end, we do need a concrete capacity.  No need to
     do a symbolic check here because the inequality check does not reason
     whether constants are greater than symbolic values.  */
  if (!cst_byte_capacity_tree)
    return oob_safe;

  bit_range buffer (0, wi::to_offset (cst_byte_capacity_tree) * BITS_PER_UNIT);
  /* If READ_BITS exceeds BUFFER, we do have an overflow.  */
  if (read_bits.exceeds_p (buffer, &bits_outside))
    {
      tree bit_bound = wide_int_to_tree (size_type_node,
					 buffer.get_next_bit_offset ());
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
							      bits_outside,
							      bit_bound));
	  oob_safe = false;
	  break;
	case DIR_WRITE:
	  ctxt->warn (make_unique<concrete_buffer_overflow> (*this,
							     reg, diag_arg,
							     bits_outside,
							     bit_bound,
							     sval_hint));
	  oob_safe = false;
	  break;
	}
    }
  return oob_safe;
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
