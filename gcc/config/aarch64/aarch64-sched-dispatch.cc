/* Dispatch scheduling hooks for AArch64.
   Copyright The GNU Toolchain Authors.

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
#include "backend.h"
#include "rtl.h"
#include "insn-attr.h"
#include "insn-attr-common.h"
#include "aarch64-protos.h"
#include "aarch64-sched-dispatch.h"
#include "regset.h"
#include "sched-int.h"
#include "dumpfile.h"
#include "print-rtl.h"

/* This file implements the target hooks for dispatch scheduling for AArch64.
   Instructions are scheduled into the current dispatch window according to
   dispatch constraints provided by the tuning model.

   To enable dispatch scheduling for a core (see Neoverse V2 for an example):
   - in the tuning model, add the AARCH64_EXTRA_TUNE_DISPATCH_SCHED tune flag
   - in the tuning model, add an array of max slot counts for each constraint
     and add its reference and length to the tune_params struct
   - in the tuning model, add a callback function to determine slot requirements
   - optionally, create a new instruction attribute to classify instructions
     into dispatch groups (e.g. neoversev2_dispatch)  */

static dispatch_window *current_dispatch_window;

/* Constructor for class dispatch_window.  */
dispatch_window::dispatch_window (const dispatch_constraint_info &constraint_info)
  : m_max_slots (constraint_info.max_slots),
    m_num_constraints (constraint_info.num_constraints),
    m_callback (constraint_info.callback),
    m_violation (false)
{
  m_free_slots = XNEWVEC (int, m_num_constraints);
  reset_constraints ();
}

/* Destructor for class dispatch_window.  */
dispatch_window::~dispatch_window ()
{
  XDELETEVEC (m_free_slots);
}

/* Return TRUE iff the given constraints fit into the dispatch window.  */
bool
dispatch_window::fits_window (rtx_insn *insn,
			      const vec<std::pair<int, int>> &constraints) const
{
  if (INSN_CODE (insn) < 0)
    return true;

  if (dump_file)
    {
      fprintf (dump_file, "Checking if insn fits into dispatch window:\n");
      print_rtl_single (dump_file, insn);
    }

  for (const auto &constraint : constraints)
    {
      if (constraint.second > m_free_slots[constraint.first])
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Constraint %d needs %d slot(s), "
		       "only %d free.\n",
		       constraint.first, constraint.second,
		       m_free_slots[constraint.first]);
	    }
	  return false;
	}
    }
  return true;
}

/* Get the constraints for an instruction.  */
vec<std::pair<int, int>>
dispatch_window::get_constraints (rtx_insn *insn) const
{
  return m_callback (insn);
}

/* Add INSN to the dispatch window and set the violation flag
   if there is a constraint violation.  */
void
dispatch_window::add_insn_to_window (rtx_insn *insn)
{
  if (INSN_CODE (insn) < 0)
    return;

  auto constraints = m_callback (insn);

  if (!fits_window (insn, constraints))
    {
      if (dump_file)
	fprintf (dump_file, "Window full. Starting new dispatch window.\n");
      reset_constraints ();
    }

  for (const auto &constraint : constraints)
    {
      m_free_slots[constraint.first] -= constraint.second;
      if (m_free_slots[constraint.first] < 0)
	m_violation = true;
    }

  if (dump_file)
    {
      fprintf (dump_file, "Insn added to dispatch window.\n");
      if (dump_flags & TDF_DETAILS)
	print_window (dump_file);
    }
}

/* Return TRUE iff there is a dispatch violation, i.e. one of the dispatch
   constraints has a negative number of free slots.  */
bool
dispatch_window::has_violation () const
{
  return m_violation;
}

/* Print information about the dispatch window to the given FILE.  */
void
dispatch_window::print_window (FILE *file) const
{
  fprintf (file, "==== Current dispatch window ====\n");
  fprintf (file, "Violation: %s\n", m_violation ? "true" : "false");
  for (int i = 0; i < m_num_constraints; i++)
    {
      fprintf (file, "Constraint %d: %d of %d slots free\n",
	       i, m_free_slots[i], m_max_slots[i]);
    }
  fprintf (file, "\n");
}

/* For all dispatch constraints, reset the number of free slots to the
   maximum number of slots.
   This is called when the next dispatch window is started.  */
void
dispatch_window::reset_constraints ()
{
  for (int i = 0; i < m_num_constraints; i++)
    m_free_slots[i] = m_max_slots[i];
  m_violation = false;
}

/* Initialize the dispatch window using the constraints from the tuning model.
   This is called once at the beginning of scheduling.  */
void
init_dispatch_window (void)
{
  const struct dispatch_constraint_info *dispatch_constraints
    = aarch64_tune_params.dispatch_constraints;
  current_dispatch_window = new dispatch_window (*dispatch_constraints);

  if (dump_file)
    {
      fprintf (dump_file, "DISPATCH WINDOW INITIALIZED\n");
      if (dump_flags & TDF_DETAILS)
	current_dispatch_window->print_window (dump_file);
    }
}

/* The next two functions implement the dispatch-scheduling target hooks
   for aarch64 and are the drivers of the dispatch scheduler.  */
void
aarch64_sched_dispatch_do (rtx_insn *insn, int mode)
{
  if (mode == DISPATCH_INIT)
    init_dispatch_window ();
  else if (mode == ADD_TO_DISPATCH_WINDOW && current_dispatch_window)
    current_dispatch_window->add_insn_to_window (insn);
}

bool
aarch64_sched_dispatch (rtx_insn *insn, int action)
{
  /* We only want dispatch scheduling to be enabled during the last
     scheduling pass, i.e. after reload and sched_fusion.  */
  if ((aarch64_tune_params.extra_tuning_flags
       & AARCH64_EXTRA_TUNE_DISPATCH_SCHED)
      && reload_completed && !sched_fusion)
    switch (action)
      {
      case IS_DISPATCH_ON:
	return true;

      /* IS_CMP may be used to delay scheduling of flag setting instructions
	 to keep them close to their consumers, e.g. branches at the end of a BB.
	 However, we don't want to delay scheduling of flag setting instructions,
	 because many consumers are not branches.  */
      case IS_CMP:
	return false;

      case DISPATCH_VIOLATION:
	return current_dispatch_window->has_violation ();

      case FITS_DISPATCH_WINDOW:
	{
	  auto constraints = current_dispatch_window->get_constraints (insn);
	  return current_dispatch_window->fits_window (insn, constraints);
	}

      default:
	return false;
      }
  return false;
}