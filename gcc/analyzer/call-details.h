/* Helper class for handling a call with specific arguments.
   Copyright (C) 2019-2023 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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

#ifndef GCC_ANALYZER_CALL_DETAILS_H
#define GCC_ANALYZER_CALL_DETAILS_H

namespace ana {

/* Helper class for handling calls to functions with known behavior.  */

class call_details
{
public:
  call_details (const gcall *call, region_model *model,
		region_model_context *ctxt);
  call_details (const call_details &cd, region_model_context *ctxt);

  region_model *get_model () const { return m_model; }
  region_model_manager *get_manager () const;
  region_model_context *get_ctxt () const { return m_ctxt; }
  logger *get_logger () const;

  uncertainty_t *get_uncertainty () const;
  tree get_lhs_type () const { return m_lhs_type; }
  const region *get_lhs_region () const { return m_lhs_region; }

  bool maybe_set_lhs (const svalue *result) const;
  void set_any_lhs_with_defaults () const;

  unsigned num_args () const;
  bool arg_is_pointer_p (unsigned idx) const
  {
    return POINTER_TYPE_P (get_arg_type (idx));
  }
  bool arg_is_size_p (unsigned idx) const;
  bool arg_is_integral_p (unsigned idx) const
  {
    return INTEGRAL_TYPE_P (get_arg_type (idx));
  }

  const gcall *get_call_stmt () const { return m_call; }
  location_t get_location () const;

  tree get_arg_tree (unsigned idx) const;
  tree get_arg_type (unsigned idx) const;
  const svalue *get_arg_svalue (unsigned idx) const;
  const region *deref_ptr_arg (unsigned idx) const;
  const char *get_arg_string_literal (unsigned idx) const;

  tree get_fndecl_for_call () const;

  void dump_to_pp (pretty_printer *pp, bool simple) const;
  void dump (bool simple) const;

  const svalue *get_or_create_conjured_svalue (const region *) const;

  tree lookup_function_attribute (const char *attr_name) const;

  void
  check_for_null_terminated_string_arg (unsigned arg_idx) const;
  const svalue *
  check_for_null_terminated_string_arg (unsigned arg_idx,
					bool include_terminator,
					const svalue **out_sval) const;

  void
  complain_about_overlap (unsigned arg_idx_a,
			  unsigned arg_idx_b,
			  const svalue *num_bytes_read_sval) const;

private:
  const gcall *m_call;
  region_model *m_model;
  region_model_context *m_ctxt;
  tree m_lhs_type;
  const region *m_lhs_region;
};

/* A bundle of information about a problematic argument at a callsite
   for use by pending_diagnostic subclasses for reporting and
   for deduplication.  */

struct call_arg_details
{
public:
  call_arg_details (const call_details &cd, unsigned arg_idx)
  : m_call (cd.get_call_stmt ()),
    m_called_fndecl (cd.get_fndecl_for_call ()),
    m_arg_idx (arg_idx),
    m_arg_expr (cd.get_arg_tree (arg_idx))
  {
  }

  bool operator== (const call_arg_details &other) const
  {
    return (m_call == other.m_call
	    && m_called_fndecl == other.m_called_fndecl
	    && m_arg_idx == other.m_arg_idx
	    && pending_diagnostic::same_tree_p (m_arg_expr, other.m_arg_expr));
  }

  const gcall *m_call;
  tree m_called_fndecl;
  unsigned m_arg_idx; // 0-based
  tree m_arg_expr;
};

} // namespace ana

#endif /* GCC_ANALYZER_CALL_DETAILS_H */
