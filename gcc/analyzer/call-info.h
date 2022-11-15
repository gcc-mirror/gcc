/* Subclasses of custom_edge_info for describing outcomes of function calls.
   Copyright (C) 2021-2022 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_CALL_INFO_H
#define GCC_ANALYZER_CALL_INFO_H

namespace ana {

/* Subclass of custom_edge_info for an outcome of a call.
   This is still abstract; the update_model and get_desc vfuncs must be
   implemented.  */

class call_info : public custom_edge_info
{
public:
  void print (pretty_printer *pp) const final override;
  void add_events_to_path (checker_path *emission_path,
			   const exploded_edge &eedge) const final override;

  const gcall *get_call_stmt () const { return m_call_stmt; }
  tree get_fndecl () const { return m_fndecl; }

  virtual label_text get_desc (bool can_colorize) const = 0;

  call_details get_call_details (region_model *model,
				 region_model_context *ctxt) const;

protected:
  call_info (const call_details &cd);

private:
  const gcall *m_call_stmt;
  tree m_fndecl;
};

/* Subclass of call_info for a "success" outcome of a call,
   adding either a
     "when `FNDECL' succeeds" message (when 'success' is true)
   or a
     "when `FNDECL' fails" message    (when 'success' is false).
   This is still abstract: the custom_edge_info::update_model vfunc
   must be implemented.  */

class succeed_or_fail_call_info : public call_info
{
public:
  label_text get_desc (bool can_colorize) const final override;

protected:
  succeed_or_fail_call_info (const call_details &cd, bool success)
   : call_info (cd), m_success (success) {}

  bool m_success;
};

/* Subclass of call_info for a "success" outcome of a call,
   adding a "when `FNDECL' succeeds" message.
   This is still abstract: the custom_edge_info::update_model vfunc
   must be implemented.  */

class success_call_info : public succeed_or_fail_call_info
{
protected:
  success_call_info (const call_details &cd)
  : succeed_or_fail_call_info (cd, true)
  {}
};

/* Subclass of call_info for a "failure" outcome of a call,
   adding a "when `FNDECL' fails" message.
   This is still abstract: the custom_edge_info::update_model vfunc
   must be implemented.  */

class failed_call_info : public succeed_or_fail_call_info
{
protected:
  failed_call_info (const call_details &cd)
  : succeed_or_fail_call_info (cd, false)
  {}
};

} // namespace ana

#endif /* GCC_ANALYZER_CALL_INFO_H */
