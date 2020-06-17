/* A state machine for use in DejaGnu tests, to check that
   pattern-matching works as expected.

   Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "tree-pretty-print.h"
#include "diagnostic-path.h"
#include "diagnostic-metadata.h"
#include "function.h"
#include "analyzer/analyzer.h"
#include "diagnostic-event-id.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"

#if ENABLE_ANALYZER

namespace ana {

namespace {

/* A state machine for use in DejaGnu tests, to check that
   pattern-matching works as expected.  */

class pattern_test_state_machine : public state_machine
{
public:
  pattern_test_state_machine (logger *logger);

  bool inherited_state_p () const FINAL OVERRIDE { return false; }

  bool on_stmt (sm_context *sm_ctxt,
		const supernode *node,
		const gimple *stmt) const FINAL OVERRIDE;

  void on_condition (sm_context *sm_ctxt,
		     const supernode *node,
		     const gimple *stmt,
		     tree lhs,
		     enum tree_code op,
		     tree rhs) const FINAL OVERRIDE;

  bool can_purge_p (state_t s) const FINAL OVERRIDE;

private:
  state_t m_start;
};

class pattern_match : public pending_diagnostic_subclass<pattern_match>
{
public:
  pattern_match (tree lhs, enum tree_code op, tree rhs)
  : m_lhs (lhs), m_op (op), m_rhs (rhs) {}

  const char *get_kind () const FINAL OVERRIDE { return "pattern_match"; }

  bool operator== (const pattern_match &other) const
  {
    return (same_tree_p (m_lhs, other.m_lhs)
	    && m_op == other.m_op
	    && same_tree_p (m_rhs, other.m_rhs));
  }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    return warning_at (rich_loc, 0, "pattern match on %<%E %s %E%>",
		       m_lhs, op_symbol_code (m_op), m_rhs);
  }

private:
  tree m_lhs;
  enum tree_code m_op;
  tree m_rhs;
};

pattern_test_state_machine::pattern_test_state_machine (logger *logger)
: state_machine ("pattern-test", logger)
{
  m_start = add_state ("start");
}

bool
pattern_test_state_machine::on_stmt (sm_context *sm_ctxt ATTRIBUTE_UNUSED,
				     const supernode *node ATTRIBUTE_UNUSED,
				     const gimple *stmt ATTRIBUTE_UNUSED) const
{
  return false;
}

/* Implementation of state_machine::on_condition vfunc for
   pattern_test_state_machine.

   Queue a pattern_match diagnostic for any comparison against a
   constant.  */

void
pattern_test_state_machine::on_condition (sm_context *sm_ctxt,
					  const supernode *node,
					  const gimple *stmt,
					  tree lhs,
					  enum tree_code op,
					  tree rhs) const
{
  if (stmt == NULL)
    return;

  if (!CONSTANT_CLASS_P (rhs))
    return;

  pending_diagnostic *diag = new pattern_match (lhs, op, rhs);
  sm_ctxt->warn_for_state (node, stmt, lhs, m_start, diag);
}

bool
pattern_test_state_machine::can_purge_p (state_t s ATTRIBUTE_UNUSED) const
{
  return true;
}

} // anonymous namespace

/* Internal interface to this file. */

state_machine *
make_pattern_test_state_machine (logger *logger)
{
  return new pattern_test_state_machine (logger);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
