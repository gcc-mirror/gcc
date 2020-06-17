/* Modeling API uses and misuses via state machines.
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
#include "options.h"
#include "function.h"
#include "diagnostic-core.h"
#include "pretty-print.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"

#if ENABLE_ANALYZER

/* If STMT is an assignment from zero, return the LHS.  */

tree
is_zero_assignment (const gimple *stmt)
{
  const gassign *assign_stmt = dyn_cast <const gassign *> (stmt);
  if (!assign_stmt)
    return NULL_TREE;

  enum tree_code op = gimple_assign_rhs_code (assign_stmt);
  if (TREE_CODE_CLASS (op) != tcc_constant)
    return NULL_TREE;

  if (!zerop (gimple_assign_rhs1 (assign_stmt)))
    return NULL_TREE;

  return gimple_assign_lhs (assign_stmt);
}

/* Return true if VAR has pointer or reference type.  */

bool
any_pointer_p (tree var)
{
  return POINTER_TYPE_P (TREE_TYPE (var));
}

namespace ana {

/* Add a state with name NAME to this state_machine.
   The string is required to outlive the state_machine.

   Return the state_t for the new state.  */

state_machine::state_t
state_machine::add_state (const char *name)
{
  m_state_names.safe_push (name);
  return m_state_names.length () - 1;
}

/* Get the name of state S within this state_machine.  */

const char *
state_machine::get_state_name (state_t s) const
{
  return m_state_names[s];
}

/* Get the state with name NAME, which must exist.
   This is purely intended for use in selftests.  */

state_machine::state_t
state_machine::get_state_by_name (const char *name)
{
  unsigned i;
  const char *iter_name;
  FOR_EACH_VEC_ELT (m_state_names, i, iter_name)
    if (!strcmp (name, iter_name))
      return i;
  /* Name not found.  */
  gcc_unreachable ();
}

/* Assert that S is a valid state for this state_machine.  */

void
state_machine::validate (state_t s) const
{
  gcc_assert (s < m_state_names.length ());
}

/* Dump a multiline representation of this state machine to PP.  */

void
state_machine::dump_to_pp (pretty_printer *pp) const
{
  unsigned i;
  const char *name;
  FOR_EACH_VEC_ELT (m_state_names, i, name)
    pp_printf (pp, "  state %i: %qs\n", i, name);
}

/* Create instances of the various state machines, each using LOGGER,
   and populate OUT with them.  */

void
make_checkers (auto_delete_vec <state_machine> &out, logger *logger)
{
  out.safe_push (make_malloc_state_machine (logger));
  out.safe_push (make_fileptr_state_machine (logger));
  /* The "taint" checker must be explicitly enabled (as it currently
     leads to state explosions that stop the other checkers working).  */
  if (flag_analyzer_checker)
    out.safe_push (make_taint_state_machine (logger));
  out.safe_push (make_sensitive_state_machine (logger));
  out.safe_push (make_signal_state_machine (logger));

  /* We only attempt to run the pattern tests if it might have been manually
     enabled (for DejaGnu purposes).  */
  if (flag_analyzer_checker)
    out.safe_push (make_pattern_test_state_machine (logger));

  if (flag_analyzer_checker)
    {
      unsigned read_index, write_index;
      state_machine **sm;

      /* TODO: this leaks the machines
	 Would be nice to log the things that were removed.  */
      VEC_ORDERED_REMOVE_IF (out, read_index, write_index, sm,
			     0 != strcmp (flag_analyzer_checker,
					  (*sm)->get_name ()));
    }
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
