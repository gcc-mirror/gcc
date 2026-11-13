/* Bounds-checking of reads and writes to memory regions.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.

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

#include "analyzer/common.h"

#include "intl.h"
#include "diagnostics/diagram.h"
#include "diagnostics/sarif-sink.h"

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
			       diagnostics::paths::event_id_t emission_id) override
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
		 const symbolic::memory_range &access_range,
		 tree diag_arg,
		 const svalue *sval_hint)
  : m_model (model),
    m_access_range (access_range),
    m_diag_arg (diag_arg),
    m_sval_hint (sval_hint)
  {}

  bool subclass_equal_p (const pending_diagnostic &base_other) const override
  {
    const out_of_bounds &other
      (static_cast <const out_of_bounds &>(base_other));
    return (m_access_range == other.m_access_range
	    && pending_diagnostic::same_tree_p (m_diag_arg, other.m_diag_arg));
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_out_of_bounds;
  }

  void mark_interesting_stuff (interesting_t *interest) final override
  {
    interest->add_region_creation (&m_access_range.get_base_region ());
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
	(std::make_unique<oob_region_creation_event_capacity> (byte_capacity,
							       loc_info,
							       *this));
  }

  void
  maybe_add_sarif_properties (diagnostics::sarif_object &result_obj)
    const override
  {
    auto &props = result_obj.get_or_create_properties ();
#define PROPERTY_PREFIX "gcc/analyzer/out_of_bounds/"
    props.set_string (PROPERTY_PREFIX "dir",
		      get_dir () == access_direction::read ? "read" : "write");
    props.set (PROPERTY_PREFIX "model", m_model.to_json ());
    props.set (PROPERTY_PREFIX "access_range", m_access_range.to_json ());
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
    return m_access_range.get_base_region ().get_memory_space ();
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
    access_operation op (m_model, get_dir (), m_access_range, m_sval_hint);

    /* Don't attempt to make a diagram if there's no valid way of
       accessing the base region (e.g. a 0-element array).  */
    if (op.get_valid_bits ().get_size ().is_zero_p ())
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
	diagnostics::diagram diagram
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
  symbolic::memory_range m_access_range;
  tree m_diag_arg;
  const svalue *m_sval_hint;
  diagnostics::paths::event_id_t m_region_creation_event_id;
};

/* Abstract base class for all out-of-bounds warnings where the
   out-of-bounds range is concrete.  */

class concrete_out_of_bounds : public out_of_bounds
{
public:
  concrete_out_of_bounds (const region_model &model,
			  const symbolic::memory_range &access_range,
			  tree diag_arg,
			  concrete_bit_range out_of_bounds_bits,
			  const svalue *sval_hint)
  : out_of_bounds (model, access_range, diag_arg, sval_hint),
    m_out_of_bounds_bits (out_of_bounds_bits)
  {}

  bool subclass_equal_p (const pending_diagnostic &base_other) const override
  {
    const concrete_out_of_bounds &other
      (static_cast <const concrete_out_of_bounds &>(base_other));
    return (out_of_bounds::subclass_equal_p (other)
	    && m_out_of_bounds_bits == other.m_out_of_bounds_bits);
  }

  void maybe_add_sarif_properties (diagnostics::sarif_object &result_obj)
    const override
  {
    out_of_bounds::maybe_add_sarif_properties (result_obj);
    auto &props = result_obj.get_or_create_properties ();
#define PROPERTY_PREFIX "gcc/analyzer/concrete_out_of_bounds/"
    props.set (PROPERTY_PREFIX "out_of_bounds_bits",
	       m_out_of_bounds_bits.to_json ());
#undef PROPERTY_PREFIX
  }

  bool get_out_of_bounds_bytes (concrete_byte_range *out) const
  {
    return maybe_bytes_from_bits (m_out_of_bounds_bits, out);
  }

protected:
  concrete_bit_range m_out_of_bounds_bits;
};

/* Abstract subclass to complain about concrete out-of-bounds
   past the end of the buffer.  */

class concrete_past_the_end : public concrete_out_of_bounds
{
public:
  concrete_past_the_end (const region_model &model,
			 const symbolic::memory_range &access_range,tree diag_arg,
			 concrete_bit_range bad_range,
			 concrete_bit_size conc_bit_capacity,
			 const svalue *sval_hint)
  : concrete_out_of_bounds (model, access_range, diag_arg, bad_range, sval_hint),
    m_conc_bit_capacity (conc_bit_capacity),
    m_conc_byte_capacity (nullptr)
  {
    concrete_byte_size bytes;
    if (maybe_bytes_from_bits (m_conc_bit_capacity, &bytes))
      m_conc_byte_capacity
	= std::make_unique<concrete_byte_size> (bytes);
  }

  bool
  subclass_equal_p (const pending_diagnostic &base_other) const final override
  {
    const concrete_past_the_end &other
      (static_cast <const concrete_past_the_end &>(base_other));
    return (concrete_out_of_bounds::subclass_equal_p (other)
	    && m_conc_bit_capacity == other.m_conc_bit_capacity);
  }

  void add_region_creation_events (const region *,
				   tree,
				   const event_loc_info &loc_info,
				   checker_path &emission_path) final override
  {
    if (m_conc_byte_capacity)
      {
	tree byte_bound
	  = make_integer_cst_from_integer (size_type_node,
					   m_conc_byte_capacity->get_integer (),
					   true);
	emission_path.add_event
	  (std::make_unique<oob_region_creation_event_capacity> (byte_bound,
								 loc_info,
								 *this));
      }
  }

  void maybe_add_sarif_properties (diagnostics::sarif_object &result_obj)
    const final override
  {
    concrete_out_of_bounds::maybe_add_sarif_properties (result_obj);
    auto &props = result_obj.get_or_create_properties ();
#define PROPERTY_PREFIX "gcc/analyzer/concrete_past_the_end/"
    props.set (PROPERTY_PREFIX "conc_bit_capacity",
	       m_conc_bit_capacity.to_json ());
    if (m_conc_byte_capacity)
      props.set (PROPERTY_PREFIX "conc_byte_capacity",
		 m_conc_byte_capacity->to_json ());
#undef PROPERTY_PREFIX
  }

protected:
  concrete_bit_size  m_conc_bit_capacity;
  std::unique_ptr<concrete_byte_size> m_conc_byte_capacity;
};

/* Concrete subclass to complain about buffer overflows.  */

class concrete_buffer_overflow : public concrete_past_the_end
{
public:
  concrete_buffer_overflow (const region_model &model,
			    const symbolic::memory_range &access_range,
			    tree diag_arg,
			    concrete_bit_range bad_range,
			    concrete_bit_size conc_bit_capacity,
			    const svalue *sval_hint)
  : concrete_past_the_end (model, access_range, diag_arg, bad_range,
			   conc_bit_capacity, sval_hint)
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
	auto num_bad_bits = m_out_of_bounds_bits.get_size ();
	concrete_byte_size num_bad_bytes;
	if (maybe_bytes_from_bits (num_bad_bits, &num_bad_bytes))
	  {
	    pp_element_integer e_num_bad_bytes (num_bad_bytes.get_integer ());
	    if (m_diag_arg)
	      inform_n (ctxt.get_location (),
			(size_t)num_bad_bytes.as_unsigned_long (),
			"write of %e byte to beyond the end of %qE",
			"write of %e bytes to beyond the end of %qE",
			&e_num_bad_bytes,
			m_diag_arg);
	    else
	      inform_n (ctxt.get_location (),
			(size_t)num_bad_bytes.as_unsigned_long (),
			"write of %e byte to beyond the end of the region",
			"write of %e bytes to beyond the end of the region",
			&e_num_bad_bytes);
	  }
	else
	  {
	    pp_element_integer e_num_bad_bits (num_bad_bits.get_integer ());
	    if (m_diag_arg)
	      inform_n (ctxt.get_location (),
			(size_t)num_bad_bits.as_unsigned_long (),
			"write of %e bit to beyond the end of %qE",
			"write of %e bits to beyond the end of %qE",
			&e_num_bad_bits,
			m_diag_arg);
	    else
	      inform_n (ctxt.get_location (),
			(size_t)num_bad_bits.as_unsigned_long (),
			"write of %e bit to beyond the end of the region",
			"write of %e bits to beyond the end of the region",
			&e_num_bad_bits);
	  }

	maybe_show_notes (ctxt);
      }

    return warned;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    if (m_conc_byte_capacity)
      {
	concrete_byte_range out_of_bounds_bytes;
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
				 const concrete_byte_range &out_of_bounds_bytes)
  {
    gcc_assert (m_conc_byte_capacity);

    auto start = out_of_bounds_bytes.get_start_offset ();
    auto last = out_of_bounds_bytes.get_last_offset ();

    pp_element_integer e_start (start.get_integer ());
    pp_element_integer e_byte_capacity (m_conc_byte_capacity->get_integer ());
    if (start == last)
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write at byte %e but %qE"
		     " ends at byte %e", &e_start, m_diag_arg,
		     &e_byte_capacity);
	else
	  pp_printf (&pp,
		     "out-of-bounds write at byte %e but region"
		     " ends at byte %e", &e_start,
		     &e_byte_capacity);
      }
    else
      {
	pp_element_integer e_last (last.get_integer ());
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write from byte %e till"
		     " byte %e but %qE ends at byte %e",
		     &e_start, &e_last, m_diag_arg,
		     &e_byte_capacity);
	else
	  pp_printf (&pp,
		     "out-of-bounds write from byte %e till"
		     " byte %e but region ends at byte %e",
		     &e_start, &e_last, &e_byte_capacity);
      }
  }

  void
  describe_final_event_as_bits (pretty_printer &pp)
  {
    auto start = m_out_of_bounds_bits.get_start_offset ();
    auto last = m_out_of_bounds_bits.get_last_offset ();

    pp_element_integer e_start (start.get_integer ());
    pp_element_integer e_bit_capacity (m_conc_bit_capacity.get_integer ());
    if (start == last)
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write at bit %e but %qE"
		     " ends at bit %e", &e_start, m_diag_arg,
		     &e_bit_capacity);
	else
	  pp_printf (&pp,
		     "out-of-bounds write at bit %e but region"
		     " ends at bit %e", &e_start,
		     &e_bit_capacity);
      }
    else
      {
	pp_element_integer e_last (last.get_integer ());
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write from bit %e till"
		     " bit %e but %qE ends at bit %e",
		     &e_start, &e_last, m_diag_arg,
		     &e_bit_capacity);
	else
	  pp_printf (&pp,
		     "out-of-bounds write from bit %e till"
		     " bit %e but region ends at bit %e",
		     &e_start, &e_last, &e_bit_capacity);
      }
  }

  enum access_direction
  get_dir () const final override { return access_direction::write; }
};

/* Concrete subclass to complain about buffer over-reads.  */

class concrete_buffer_over_read : public concrete_past_the_end
{
public:
  concrete_buffer_over_read (const region_model &model,
			     const symbolic::memory_range &access_range,
			     tree diag_arg,
			     concrete_bit_range bad_range,
			     concrete_bit_size conc_bit_capacity)
  : concrete_past_the_end (model, access_range, diag_arg,
			   bad_range, conc_bit_capacity, nullptr)
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
	auto num_bad_bits = m_out_of_bounds_bits.get_size ();
	concrete_byte_size num_bad_bytes;
	if (maybe_bytes_from_bits (num_bad_bits, &num_bad_bytes))
	  {
	    pp_element_integer e_num_bad_bytes (num_bad_bytes.get_integer ());
	    if (m_diag_arg)
	      inform_n (ctxt.get_location (),
			(size_t)num_bad_bytes.as_unsigned_long (),
			"read of %e byte from after the end of %qE",
			"read of %e bytes from after the end of %qE",
			&e_num_bad_bytes,
			m_diag_arg);
	    else
	      inform_n (ctxt.get_location (),
			(size_t)num_bad_bytes.as_unsigned_long (),
			"read of %e byte from after the end of the region",
			"read of %e bytes from after the end of the region",
			&e_num_bad_bytes);
	  }
	else
	  {
	    pp_element_integer e_num_bad_bits (num_bad_bits.get_integer ());
	    if (m_diag_arg)
	      inform_n (ctxt.get_location (),
			(size_t)num_bad_bits.as_unsigned_long (),
			"read of %e bit from after the end of %qE",
			"read of %e bits from after the end of %qE",
			&e_num_bad_bits,
			m_diag_arg);
	    else
	      inform_n (ctxt.get_location (),
			(size_t)num_bad_bits.as_unsigned_long (),
			"read of %e bit from after the end of the region",
			"read of %e bits from after the end of the region",
			&e_num_bad_bits);
	  }

	maybe_show_notes (ctxt);
      }

    return warned;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    if (m_conc_byte_capacity)
      {
	concrete_byte_range out_of_bounds_bytes;
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
				 const concrete_byte_range &out_of_bounds_bytes)
  {
    gcc_assert (m_conc_byte_capacity);

    auto start = out_of_bounds_bytes.get_start_offset ();
    auto last = out_of_bounds_bytes.get_last_offset ();

    pp_element_integer e_start (start.get_integer ());
    pp_element_integer e_byte_capacity (m_conc_byte_capacity->get_integer ());

    if (start == last)
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read at byte %e but %qE"
		     " ends at byte %e", &e_start, m_diag_arg,
		     &e_byte_capacity);
	else
	  pp_printf (&pp,
		     "out-of-bounds read at byte %e but region"
		     " ends at byte %e", &e_start,
		     &e_byte_capacity);
      }
    else
      {
	pp_element_integer e_last (last.get_integer ());
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read from byte %e till"
		     " byte %e but %qE ends at byte %e",
		     &e_start, &e_last, m_diag_arg,
		     &e_byte_capacity);
	else
	  pp_printf (&pp,
		     "out-of-bounds read from byte %e till"
		     " byte %e but region ends at byte %e",
		     &e_start, &e_last, &e_byte_capacity);
      }
  }

  void
  describe_final_event_as_bits (pretty_printer &pp)
  {
    auto start = m_out_of_bounds_bits.get_start_offset ();
    auto last = m_out_of_bounds_bits.get_last_offset ();

    pp_element_integer e_start (start.get_integer ());
    pp_element_integer e_bit_capacity (m_conc_bit_capacity.get_integer ());

    if (start == last)
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read at bit %e but %qE"
		     " ends at bit %e", &e_start, m_diag_arg,
		     &e_bit_capacity);
	else
	  pp_printf (&pp,
		     "out-of-bounds read at bit %e but region"
		     " ends at bit %e", &e_start,
		     &e_bit_capacity);
      }
    else
      {
	pp_element_integer e_last (last.get_integer ());
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read from bit %e till"
		     " bit %e but %qE ends at bit %e",
		     &e_start, &e_last, m_diag_arg,
		     &e_bit_capacity);
	else
	  pp_printf (&pp,
		     "out-of-bounds read from bit %e till"
		     " bit %e but region ends at bit %e",
		     &e_start, &e_last, &e_bit_capacity);
      }
  }

  enum access_direction
  get_dir () const final override { return access_direction::read; }
};

/* Concrete subclass to complain about buffer underwrites.  */

class concrete_buffer_underwrite : public concrete_out_of_bounds
{
public:
  concrete_buffer_underwrite (const region_model &model,
			      const symbolic::memory_range &access_range,
			      tree diag_arg,
			      concrete_bit_range bad_range,
			      const svalue *sval_hint)
  : concrete_out_of_bounds (model, access_range, diag_arg,
			    bad_range, sval_hint)
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
    concrete_byte_range out_of_bounds_bytes;
    if (get_out_of_bounds_bytes (&out_of_bounds_bytes))
      describe_final_event_as_bytes (pp, out_of_bounds_bytes);
    else
      describe_final_event_as_bits (pp);
    return true;
  }

  void
  describe_final_event_as_bytes (pretty_printer &pp,
				 const concrete_byte_range &out_of_bounds_bytes)
  {
    auto start = out_of_bounds_bytes.get_start_offset ();
    auto last = out_of_bounds_bytes.get_last_offset ();

    pp_element_integer e_start (start.get_integer ());

    if (start == last)
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write at byte %e but %qE"
		     " starts at byte 0",
		     &e_start, m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds write at byte %e but region"
		     " starts at byte 0", &e_start);
      }
    else
      {
	pp_element_integer e_last (last.get_integer ());
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write from byte %e till"
		     " byte %e but %qE starts at byte 0",
		     &e_start, &e_last, m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds write from byte %e till"
		     " byte %e but region starts at byte 0",
		     &e_start, &e_last);
      }
  }

  void
  describe_final_event_as_bits (pretty_printer &pp)
  {
    auto start = m_out_of_bounds_bits.get_start_offset ();
    auto last = m_out_of_bounds_bits.get_last_offset ();

    pp_element_integer e_start (start.get_integer ());
    if (start == last)
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write at bit %e but %qE"
		     " starts at bit 0",
		     &e_start, m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds write at bit %e but region"
		     " starts at bit 0", &e_start);
      }
    else
      {
	pp_element_integer e_last (last.get_integer ());
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds write from bit %e till"
		     " bit %e but %qE starts at bit 0",
		     &e_start, &e_last, m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds write from bit %e till"
		     " bit %e but region starts at bit 0",
		     &e_start, &e_last);
      }
  }

  enum access_direction
  get_dir () const final override { return access_direction::write; }
};

/* Concrete subclass to complain about buffer under-reads.  */

class concrete_buffer_under_read : public concrete_out_of_bounds
{
public:
  concrete_buffer_under_read (const region_model &model,
			      const symbolic::memory_range &access_range,
			      tree diag_arg,
			      concrete_bit_range bad_range)
  : concrete_out_of_bounds (model, access_range, diag_arg, bad_range, nullptr)
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
    concrete_byte_range out_of_bounds_bytes;
    if (get_out_of_bounds_bytes (&out_of_bounds_bytes))
      describe_final_event_as_bytes (pp, out_of_bounds_bytes);
    else
      describe_final_event_as_bits (pp);
    return true;
  }

  void
  describe_final_event_as_bytes (pretty_printer &pp,
				 const concrete_byte_range &out_of_bounds_bytes)
  {
    auto start = out_of_bounds_bytes.get_start_offset ();
    auto last = out_of_bounds_bytes.get_last_offset ();

    pp_element_integer e_start (start.get_integer ());
    if (start == last)
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read at byte %e but %qE"
		     " starts at byte 0",
		     &e_start, m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds read at byte %e but region"
		     " starts at byte 0",
		     &e_start);
      }
    else
      {
	pp_element_integer e_last (last.get_integer ());
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read from byte %e till"
		     " byte %e but %qE starts at byte 0",
		     &e_start, &e_last, m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds read from byte %e till"
		     " byte %e but region starts at byte 0",
		     &e_start, &e_last);
      }
  }

  void
  describe_final_event_as_bits (pretty_printer &pp)
  {
    auto start = m_out_of_bounds_bits.get_start_offset ();
    auto last = m_out_of_bounds_bits.get_last_offset ();

    pp_element_integer e_start (start.get_integer ());

    if (start == last)
      {
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read at bit %e but %qE"
		     " starts at bit 0",
		     &e_start, m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds read at bit %e but region"
		     " starts at bit 0", &e_start);
      }
    else
      {
	pp_element_integer e_last (last.get_integer ());
	if (m_diag_arg)
	  pp_printf (&pp,
		     "out-of-bounds read from bit %e till"
		     " bit %e but %qE starts at bit 0",
		     &e_start, &e_last, m_diag_arg);
	else
	  pp_printf (&pp,
		     "out-of-bounds read from bit %e till"
		     " bit %e but region starts at bit 0",
		     &e_start, &e_last);
      }
  }

  enum access_direction
  get_dir () const final override { return access_direction::read; }
};

/* Abstract class to complain about out-of-bounds read/writes where
   the values are symbolic.  */

class symbolic_past_the_end : public out_of_bounds
{
public:
  symbolic_past_the_end (const region_model &model,
			 const symbolic::memory_range &access_range,
			 tree diag_arg,
			 tree offset,
			 tree num_bytes,
			 tree capacity,
			 const svalue *sval_hint)
  : out_of_bounds (model, access_range, diag_arg, sval_hint),
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

  void
  maybe_add_sarif_properties (diagnostics::sarif_object &result_obj)
    const final override
  {
    out_of_bounds::maybe_add_sarif_properties (result_obj);
    auto &props = result_obj.get_or_create_properties ();
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
			    const symbolic::memory_range &access_range,
			    tree diag_arg,
			    tree offset,
			    tree num_bytes,
			    tree capacity,
			    const svalue *sval_hint)
  : symbolic_past_the_end (model, access_range, diag_arg,
			   offset, num_bytes,
			   capacity,
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

  enum access_direction
  get_dir () const final override { return access_direction::write; }
};

/* Concrete subclass to complain about over-reads with symbolic values.  */

class symbolic_buffer_over_read : public symbolic_past_the_end
{
public:
  symbolic_buffer_over_read (const region_model &model,
			     const symbolic::memory_range &access_range,
			     tree diag_arg,
			     tree offset,
			     tree num_bytes,
			     tree capacity)
  : symbolic_past_the_end (model, access_range, diag_arg,
			   offset, num_bytes,
			   capacity,
			   nullptr)
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

  enum access_direction
  get_dir () const final override { return access_direction::read; }
};

/* Check whether an access is past the end of the BASE_REG.
   Return TRUE if the access was valid, FALSE otherwise.  */

bool
region_model::check_symbolic_bounds (const symbolic::memory_range &attempted_range,
				     const svalue *capacity,
				     enum access_direction dir,
				     const svalue *sval_hint,
				     region_model_context *ctxt) const
{
  gcc_assert (ctxt);

  auto &next_bit = attempted_range.get_bit_range ().get_next_offset ();
  auto *next_byte_as_scalar = maybe_bytes_from_bits (next_bit);
  if (!next_byte_as_scalar)
    return true;
  
  auto &start_bit = attempted_range.get_bit_range ().get_start_offset ();
  auto *start_byte_as_scalar = maybe_bytes_from_bits (start_bit);
  if (!start_byte_as_scalar)
    return true;

  auto &attempted_size_in_bits = attempted_range.get_bit_range ().get_size ();
  auto *attempted_size_in_bytes
    = maybe_bytes_from_bits (attempted_size_in_bits);
  if (!attempted_size_in_bytes)
    return true;

  const svalue *next_byte
    = m_mgr->get_svalue_from_scalar (size_type_node, *next_byte_as_scalar);

  if (eval_condition (next_byte, GT_EXPR, capacity).is_true ())
    {
      auto &base_reg = attempted_range.get_base_region ();
      tree diag_arg = get_representative_tree (&base_reg);
      const svalue *sym_byte_offset
	= m_mgr->get_svalue_from_scalar (size_type_node, *start_byte_as_scalar);
      tree offset_tree = get_representative_tree (sym_byte_offset);
      const svalue *num_bytes_sval
	= m_mgr->get_svalue_from_scalar (size_type_node,
					 *attempted_size_in_bytes);
      tree num_bytes_tree = get_representative_tree (num_bytes_sval);
      tree capacity_tree = get_representative_tree (capacity);
      switch (dir)
	{
	default:
	  gcc_unreachable ();
	  break;
	case access_direction::read:
	  gcc_assert (sval_hint == nullptr);
	  ctxt->warn
	    (std::make_unique<symbolic_buffer_over_read> (*this,
							  attempted_range,
							  diag_arg,
							  offset_tree,
							  num_bytes_tree,
							  capacity_tree));
	  return false;
	  break;
	case access_direction::write:
	  ctxt->warn
	    (std::make_unique<symbolic_buffer_overflow> (*this,
							 attempted_range,
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

/* Complain to CTXT if an access of ATTEMPTED_RANGE is outside the bounds
   of the underlying region.  SVAL_HINT can be non-null for writes and, if
   so, may be used to make the diagnostic more readable.
   Return true if the access was valid, false otherwise.  */

bool
region_model::
check_memory_range_bounds (const symbolic::memory_range &attempted_range,
			   enum access_direction dir,
			   const svalue *sval_hint,
			   region_model_context *ctxt) const
{
  gcc_assert (ctxt);

  /* Bail out if 0 bits are accessed.  */
  if (attempted_range.get_bit_range ().get_size ().is_zero_p ())
    return true;

  const region &base_reg = attempted_range.get_base_region ();

  /* Get the capacity of the buffer (in bytes).  */
  auto &byte_capacity = get_capacity_as_scalar (&base_reg);

  integer_with_unit<units::byte> conc_byte_capacity;
  bool capacity_is_concrete
    = concrete_from_symbolic (byte_capacity, &conc_byte_capacity);

  /* If any of the base region, the offset, or the number of bytes accessed
     are symbolic, we have to reason about symbolic values.  */

  concrete_bit_range conc_attempted_range;
  bool attempted_range_is_concrete
    = concrete_from_symbolic (attempted_range.get_bit_range (),
			      &conc_attempted_range);
  if (base_reg.symbolic_p () || !attempted_range_is_concrete)
    {
      auto *byte_capacity_as_sval = get_capacity (&base_reg);
      gcc_assert (byte_capacity_as_sval);
      return check_symbolic_bounds (attempted_range,
				    byte_capacity_as_sval,
				    dir, sval_hint, ctxt);
    }

  /* Otherwise continue to check with concrete values.  */
  concrete_bit_range bits_outside;
  bool oob_safe = true;
  /* If conc_attempted_range has a subset < 0, we do have an underwrite.  */
  if (conc_attempted_range.falls_short_of_p (0_bits, &bits_outside))
    {
      tree diag_arg = get_representative_tree (&base_reg);
      switch (dir)
	{
	default:
	  gcc_unreachable ();
	  break;
	case access_direction::read:
	  gcc_assert (sval_hint == nullptr);
	  ctxt->warn
	    (std::make_unique<concrete_buffer_under_read> (*this,
							   attempted_range,
							   diag_arg,
							   bits_outside));
	  oob_safe = false;
	  break;
	case access_direction::write:
	  ctxt->warn
	    (std::make_unique<concrete_buffer_underwrite> (*this,
							   attempted_range,
							   diag_arg,
							   bits_outside,
							   sval_hint));
	  oob_safe = false;
	  break;
	}
    }

  /* For accesses past the end, we do need a concrete capacity.  No need to
     do a symbolic check here because the inequality check does not reason
     whether constants are greater than symbolic values.  */
  if (!capacity_is_concrete)
    return oob_safe;

  auto conc_bit_capacity = bits_from_bytes (conc_byte_capacity);
  if (conc_attempted_range.exceeds_p (conc_bit_capacity, &bits_outside))
    {
      tree diag_arg = get_representative_tree (&base_reg);

      switch (dir)
	{
	default:
	  gcc_unreachable ();
	  break;
	case access_direction::read:
	  gcc_assert (sval_hint == nullptr);
	  ctxt->warn
	    (std::make_unique<concrete_buffer_over_read> (*this,
							  attempted_range,
							  diag_arg,
							  bits_outside,
							  conc_bit_capacity));
	  oob_safe = false;
	  break;
	case access_direction::write:
	  ctxt->warn
	    (std::make_unique<concrete_buffer_overflow> (*this,
							 attempted_range,
							 diag_arg,
							 bits_outside,
							 conc_bit_capacity,
							 sval_hint));
	  oob_safe = false;
	  break;
	}
    }
  return oob_safe;
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

  return check_memory_range_bounds (reg->get_memory_range (*m_mgr),
				    dir,
				    sval_hint,
				    ctxt);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
