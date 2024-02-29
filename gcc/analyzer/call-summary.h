/* Classes for working with summaries of function calls.
   Copyright (C) 2022 David Malcolm <dmalcolm@redhat.com>.

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

#ifndef GCC_ANALYZER_CALL_SUMMARY_H
#define GCC_ANALYZER_CALL_SUMMARY_H

#include "call-details.h"

namespace ana {

/* A class summarizing one particular outcome of a function that
   we've already analyzed.
   This lets us efficiently replay the analysis when we see calls
   to the function, providing an approximation of the behavior of
   the function without having to execute within the function itself.  */

class call_summary
{
public:
  call_summary (per_function_data *per_fn_data,
		const exploded_node *enode)
  : m_per_fn_data (per_fn_data),
    m_enode (enode)
  {}
  const program_state &get_state () const;
  tree get_fndecl () const;

  label_text get_desc () const;

  void dump_to_pp (const extrinsic_state &ext_state,
		   pretty_printer *pp,
		   bool simple) const;
  void dump (const extrinsic_state &ext_state, FILE *fp, bool simple) const;
  void dump (const extrinsic_state &ext_state, bool simple) const;

private:
  void get_user_facing_desc (pretty_printer *pp) const;

  per_function_data *const m_per_fn_data;
  const exploded_node *const m_enode;
};

/* A class for handling replaying a specific call summary at
   a specific call site.

   Supports remapping svalues and regions, e.g. remapping
     INIT_VAL(param of callee)
   to:
     whatever that argument is at the call site.  */

class call_summary_replay
{
public:
  call_summary_replay (const call_details &cd,
		       const function &called_fn,
		       call_summary *m_summary,
		       const extrinsic_state &ext_state);

  const call_details &get_call_details () const { return m_cd; }
  const gcall *get_call_stmt () const { return m_cd.get_call_stmt (); }
  region_model_manager *get_manager () const { return m_cd.get_manager (); }
  store_manager *get_store_manager () const
  {
    return get_manager ()->get_store_manager ();
  }
  region_model_context *get_ctxt () const { return m_cd.get_ctxt (); }
  region_model *get_caller_model () const { return m_cd.get_model (); }

  const svalue *convert_svalue_from_summary (const svalue *);
  const region *convert_region_from_summary (const region *);
  const binding_key *convert_key_from_summary (const binding_key *);

  void add_svalue_mapping (const svalue *summary_sval,
			   const svalue *caller_sval);
  void add_region_mapping (const region *summary_sval,
			   const region *caller_sval);

  void dump_to_pp (pretty_printer *pp, bool simple) const;
  void dump (FILE *fp, bool simple) const;
  void dump (bool simple) const;

private:
  DISABLE_COPY_AND_ASSIGN (call_summary_replay);

  const svalue *convert_svalue_from_summary_1 (const svalue *);
  const region *convert_region_from_summary_1 (const region *);

  const call_details &m_cd;
  call_summary *m_summary;
  const extrinsic_state &m_ext_state;

  // Mapping from svalues in summary to svalues for callsite:
  typedef hash_map <const svalue *, const svalue *> svalue_map_t;
  svalue_map_t m_map_svalue_from_summary_to_caller;

  // Mapping from regions in summary to regions for callsite:
  typedef hash_map <const region *, const region *> region_map_t;
  region_map_t m_map_region_from_summary_to_caller;
};

} // namespace ana

#endif /* GCC_ANALYZER_CALL_SUMMARY_H */
