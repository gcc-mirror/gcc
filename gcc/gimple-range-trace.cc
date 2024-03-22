/* Code for GIMPLE range trace and debugging related routines.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>
   and Aldy Hernandez <aldyh@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "fold-const.h"
#include "tree-cfg.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "gimple-range.h"


// Breakpoint to trap at a specific index.  From GDB, this provides a simple
// place to put a breakpoint to stop at a given trace line.
// ie.  b range_tracer::breakpoint if index == 45678

void
range_tracer::breakpoint (unsigned index ATTRIBUTE_UNUSED)
{
}

// Construct a range_tracer with component NAME.

range_tracer::range_tracer (const char *name)
{
  gcc_checking_assert (strlen(name) < name_len -1);
  strcpy (component, name);
  indent = 0;
  tracing = false;
}

// This routine does the initial line spacing/indenting for a trace.
// If BLANKS is false, then IDX is printed, otherwise spaces.

void
range_tracer::print_prefix (unsigned idx, bool blanks)
{
  // Print counter index as well as INDENT spaces.
  if (!blanks)
    fprintf (dump_file, "%-7u ", idx);
  else
    fprintf (dump_file, "        ");
  fprintf (dump_file, "%s ", component);
  unsigned x;
  for (x = 0; x< indent; x++)
    fputc (' ', dump_file);

}
// If dumping, return the next call index and print the prefix for the next
// output line.  If not, return 0.
// Counter is static to monotonically increase across the compilation unit.

unsigned
range_tracer::do_header (const char *str)
{
  static unsigned trace_count = 0;

  unsigned idx = ++trace_count;
  print_prefix (idx, false);
  fprintf (dump_file, "%s", str);
  indent += bump;
  breakpoint (idx);
  return idx;
}

// Print a line without starting or ending a trace.

void
range_tracer::print (unsigned counter, const char *str)
{
  print_prefix (counter, true);
  fprintf (dump_file, "%s", str);
}

// End a trace and print the CALLER, NAME, and RESULT and range R,

void
range_tracer::trailer (unsigned counter, const char *caller, bool result,
		      tree name, const vrange &r)
{
  gcc_checking_assert (tracing && counter != 0);

  indent -= bump;
  print_prefix (counter, true);
  fputs(result ? "TRUE : " : "FALSE : ", dump_file);
  fprintf (dump_file, "(%u) ", counter);
  fputs (caller, dump_file);
  fputs (" (",dump_file);
  if (name)
    print_generic_expr (dump_file, name, TDF_SLIM);
  fputs (") ",dump_file);
  if (result)
    {
      r.dump (dump_file);
      fputc('\n', dump_file);
    }
  else
    fputc('\n', dump_file);
}

// =========================================
// Debugging helpers.
// =========================================

// Query all statements in the IL to precalculate computable ranges in RANGER.

DEBUG_FUNCTION void
debug_seed_ranger (gimple_ranger &ranger)
{
  // Recalculate SCEV to make sure the dump lists everything.
  if (scev_initialized_p ())
    {
      scev_finalize ();
      scev_initialize ();
    }

  basic_block bb;
  gimple_stmt_iterator gsi;
  FOR_EACH_BB_FN (bb, cfun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);

	if (is_gimple_debug (stmt))
	  continue;

	if (tree type = gimple_range_type (stmt))
	  {
	    Value_Range r (type);
	    ranger.range_of_stmt (r, stmt);
	  }
      }
}

// Change the current dump_file and dump_flags to F and FLAGS while
// saving them for later restoring.

push_dump_file::push_dump_file (FILE *f, dump_flags_t flags)
{
  old_dump_file = dump_file;
  old_dump_flags = dump_flags;
  dump_file = f;
  dump_flags = flags;
}

// Restore saved dump_file and dump_flags.

push_dump_file::~push_dump_file ()
{
  dump_file = old_dump_file;
  dump_flags = old_dump_flags;
}

// Dump all that ranger knows for the current function.

void
dump_ranger (FILE *out)
{
  push_dump_file save (out, dump_flags);
  gimple_ranger ranger;

  fprintf (out, ";; Function ");
  print_generic_expr (out, current_function_decl);
  fprintf (out, "\n");

  debug_seed_ranger (ranger);
  ranger.dump (out);
}

DEBUG_FUNCTION void
debug_ranger ()
{
  dump_ranger (stderr);
}

// Dump all that ranger knows on a path of BBs.
//
// Note that the blocks are in reverse order, thus the exit block is
// path[0].

void
dump_ranger (FILE *dump_file, const vec<basic_block> &path)
{
  if (path.length () == 0)
    {
      fprintf (dump_file, "empty\n");
      return;
    }

  gimple_ranger ranger;
  debug_seed_ranger (ranger);

  unsigned i = path.length ();
  do
    {
      i--;
      ranger.dump_bb (dump_file, path[i]);
    }
  while (i > 0);
}

DEBUG_FUNCTION void
debug_ranger (const vec<basic_block> &path)
{
  dump_ranger (stderr, path);
}

#include "gimple-range-tests.cc"
