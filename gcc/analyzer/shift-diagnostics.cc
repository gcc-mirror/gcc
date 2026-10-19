/* Diagnostics for complaining about shift operations.
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
#include "analyzer/feasible-graph.h"
#include "diagnostics/sarif-sink.h"

#if ENABLE_ANALYZER

namespace ana {

/* A subclass of pending_diagnostic for complaining about shifts
   by negative counts.  */

class shift_count_negative_diagnostic
: public pending_diagnostic_subclass<shift_count_negative_diagnostic>
{
public:
  shift_count_negative_diagnostic (const gassign *assign, tree count_cst,
				   const region *src_region)
  : m_assign (assign), m_count_cst (count_cst), m_src_region (src_region)
  {}

  const char *get_kind () const final override
  {
    return "shift_count_negative_diagnostic";
  }

  bool operator== (const shift_count_negative_diagnostic &other) const
  {
    return (m_assign == other.m_assign
	    && same_tree_p (m_count_cst, other.m_count_cst));
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_shift_count_negative;
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    return ctxt.warn ("shift by negative count (%qE)", m_count_cst);
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    pp_printf (&pp,
	       "shift by negative amount here (%qE)",
	       m_count_cst);
    return true;
  }

  void
  mark_interesting_stuff (interesting_t *interest) override
  {
    interest->add_read_region (m_src_region, "shift count value");
  }

private:
  const gassign *m_assign;
  tree m_count_cst;
  const region *m_src_region;
};

std::unique_ptr<pending_diagnostic>
make_shift_count_negative_diagnostic (const gassign *assign, tree count_cst,
				      const region *src_region)
{
  return std::make_unique<shift_count_negative_diagnostic>
    (assign, count_cst, src_region);
}

/* A subclass of pending_diagnostic for complaining about shifts
   by counts >= the width of the operand type.  */

class shift_count_overflow_diagnostic
: public pending_diagnostic_subclass<shift_count_overflow_diagnostic>
{
public:
  shift_count_overflow_diagnostic (const gassign *assign,
				   int operand_precision,
				   tree count_cst,
				   const region *src_region)
  : m_assign (assign), m_operand_precision (operand_precision),
    m_count_cst (count_cst),
    m_src_region (src_region)
  {}

  const char *get_kind () const final override
  {
    return "shift_count_overflow_diagnostic";
  }

  bool operator== (const shift_count_overflow_diagnostic &other) const
  {
    return (m_assign == other.m_assign
	    && m_operand_precision == other.m_operand_precision
	    && same_tree_p (m_count_cst, other.m_count_cst));
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_shift_count_overflow;
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    return ctxt.warn ("shift by count (%qE) >= precision of type (%qi)",
		      m_count_cst, m_operand_precision);
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    pp_printf (&pp,
	       "shift by count %qE here",
	       m_count_cst);
    return true;
  }

  void
  mark_interesting_stuff (interesting_t *interest) override
  {
    interest->add_read_region (m_src_region, "shift count value");
  }

private:
  const gassign *m_assign;
  int m_operand_precision;
  tree m_count_cst;
  const region *m_src_region;
};

std::unique_ptr<pending_diagnostic>
make_shift_count_overflow_diagnostic (const gassign *assign,
				      int operand_precision,
				      tree count_cst,
				      const region *src_region)
{
  return std::make_unique<shift_count_overflow_diagnostic>
    (assign, operand_precision, count_cst, src_region);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
