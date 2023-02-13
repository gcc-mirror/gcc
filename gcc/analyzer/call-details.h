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

  region_model *get_model () const { return m_model; }
  region_model_manager *get_manager () const;
  region_model_context *get_ctxt () const { return m_ctxt; }
  logger *get_logger () const;

  uncertainty_t *get_uncertainty () const;
  tree get_lhs_type () const { return m_lhs_type; }
  const region *get_lhs_region () const { return m_lhs_region; }

  bool maybe_set_lhs (const svalue *result) const;

  unsigned num_args () const;
  bool arg_is_pointer_p (unsigned idx) const
  {
    return POINTER_TYPE_P (get_arg_type (idx));
  }
  bool arg_is_size_p (unsigned idx) const;

  const gcall *get_call_stmt () const { return m_call; }
  location_t get_location () const;

  tree get_arg_tree (unsigned idx) const;
  tree get_arg_type (unsigned idx) const;
  const svalue *get_arg_svalue (unsigned idx) const;
  const char *get_arg_string_literal (unsigned idx) const;

  tree get_fndecl_for_call () const;

  void dump_to_pp (pretty_printer *pp, bool simple) const;
  void dump (bool simple) const;

  const svalue *get_or_create_conjured_svalue (const region *) const;

private:
  const gcall *m_call;
  region_model *m_model;
  region_model_context *m_ctxt;
  tree m_lhs_type;
  const region *m_lhs_region;
};

} // namespace ana

#endif /* GCC_ANALYZER_CALL_DETAILS_H */
