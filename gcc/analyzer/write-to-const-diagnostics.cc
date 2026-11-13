/* Implementation of write_to_const_diagnostic and
   write_to_string_literal_diagnostic.
   Copyright (C) 2020-2026 Free Software Foundation, Inc.
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

#include "analyzer/common.h"

#include "analyzer/region-model.h"

#if ENABLE_ANALYZER

namespace ana {

/* A subclass of pending_diagnostic for complaining about writes to
   constant regions of memory.  */

class write_to_const_diagnostic
: public pending_diagnostic_subclass<write_to_const_diagnostic>
{
public:
  write_to_const_diagnostic (const region *base_reg, tree decl)
  : m_base_reg (base_reg), m_decl (decl)
  {}

  const char *get_kind () const final override
  {
    return "write_to_const_diagnostic";
  }

  bool operator== (const write_to_const_diagnostic &other) const
  {
    return (m_base_reg == other.m_base_reg
	    && m_decl == other.m_decl);
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_write_to_const;
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    auto_diagnostic_group d;
    bool warned;
    switch (m_base_reg->get_kind ())
      {
      default:
	warned = ctxt.warn ("write to %<const%> object %qE", m_decl);
	break;
      case RK_FUNCTION:
	warned = ctxt.warn ("write to function %qE", m_decl);
	break;
      case RK_LABEL:
	warned = ctxt.warn ("write to label %qE", m_decl);
	break;
      }
    if (warned)
      inform (DECL_SOURCE_LOCATION (m_decl), "declared here");
    return warned;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    switch (m_base_reg->get_kind ())
      {
      default:
	{
	  pp_printf (&pp,
		     "write to %<const%> object %qE here", m_decl);
	  return true;
	}
      case RK_FUNCTION:
	{
	  pp_printf (&pp,
		     "write to function %qE here", m_decl);
	  return true;
	}
      case RK_LABEL:
	{
	  pp_printf (&pp,
		     "write to label %qE here", m_decl);
	  return true;
	}
      }
  }

private:
  const region *m_base_reg;
  tree m_decl;
};

std::unique_ptr<pending_diagnostic>
make_write_to_const_diagnostic (const region *base_reg, tree decl)
{
  return std::make_unique<write_to_const_diagnostic> (base_reg, decl);
}

/* A subclass of pending_diagnostic for complaining about writes to
   string literals.  */

class write_to_string_literal_diagnostic
: public pending_diagnostic_subclass<write_to_string_literal_diagnostic>
{
public:
  write_to_string_literal_diagnostic (const region *base_reg)
  : m_base_reg (base_reg)
  {}

  const char *get_kind () const final override
  {
    return "write_to_string_literal_diagnostic";
  }

  bool operator== (const write_to_string_literal_diagnostic &other) const
  {
    return m_base_reg == other.m_base_reg;
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_write_to_string_literal;
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    return ctxt.warn ("write to string literal");
    /* Ideally we would show the location of the STRING_CST as well,
       but it is not available at this point.  */
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    pp_string (&pp, "write to string literal here");
    return true;
  }

private:
  const region *m_base_reg;
};

std::unique_ptr<pending_diagnostic>
make_write_to_string_literal_diagnostic (const region *base_reg)
{
  return std::make_unique<write_to_string_literal_diagnostic> (base_reg);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
