// Miscellaneous support routines not intended for upstream merge.

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "cfganal.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-propagate.h"
#include "alloc-pool.h"
#include "domwalk.h"
#include "tree-cfgcleanup.h"
#include "vr-values.h"
#include "gimple-ssa-evrp-analyze.h"
#include "fold-const.h"
#include "misc.h"

//====================================================
// unseen_function_p
//====================================================

static const char *
get_fresh_function_name ()
{
  char *result;
  if (cfun->decl)
    {
      pretty_printer pp;
      dump_generic_node (&pp, cfun->decl, 0, TDF_VOPS|TDF_MEMSYMS, false);
      const char *str = pp_formatted_text (&pp);
      result = new char[strlen (str) + 1];
      return strcpy (result, str);
    }
  result = new char[sizeof("UNKNOWN") + 1];
  return strcpy (result, "UNKNOWN");
}

static bool
unseen_function_p ()
{
  bool unseen = true;
  static const char *prev_function_name;
  const char *curr_function_name = get_fresh_function_name ();

  if (prev_function_name && curr_function_name)
    unseen = strcmp (prev_function_name, curr_function_name);

  if (prev_function_name)
    delete prev_function_name;
  prev_function_name = curr_function_name;
  return unseen;
}

//====================================================
// highlighter
//====================================================

class highlighter highlighter;

#define INDENT(SPACE)							\
  do { int i; for (i = 0; i < SPACE; i++) pp_space (buffer); } while (0)

void
highlighter::on (pretty_printer *buffer, int spc, gimple *stmt)
{
  bool need_header = new_stmt == stmt || untainted_stmt == stmt;
  if (need_header)
    {
      pp_string (buffer, ";; (STATE) filename = ");
      pp_string (buffer, main_input_filename);
      pp_newline_and_flush (buffer);
      INDENT (spc);
    }
  if (new_stmt == stmt)
    {
      pp_string (buffer, ";; Original statement was: ");
      pp_gimple_stmt_1 (buffer, old_stmt, spc, TDF_SLIM);
    }
  else if (untainted_stmt == stmt)
    {
      pp_string (buffer, ";; Queued for removal LHS= ");
      dump_generic_node (buffer, lhs, spc, TDF_SLIM, false);
    }
  if (need_header)
    {
      pp_newline_and_flush (buffer);
      INDENT (spc);
      pp_string (buffer, ";; VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV\n");
      INDENT (spc);
    }
}

void
highlighter::off (pretty_printer *buffer, int spc, gimple *stmt)
{
  if (new_stmt == stmt || untainted_stmt == stmt)
    {
      pp_newline_and_flush (buffer);
      INDENT (spc);
      pp_string (buffer, ";; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^");
    }
}

void
highlighter::set (gimple *untainted_stmt,
		  gimple *old_stmt, gimple *new_stmt, tree lhs)
{
  this->untainted_stmt = untainted_stmt;
  this->old_stmt = old_stmt;
  this->new_stmt = new_stmt;
  this->lhs = lhs;
}

//====================================================
// gimple_state
//====================================================

gimple_state::gimple_state ()
{
  orig_stmt = NULL;
  out = stderr;
  accumulate_changes = false;

  // Instead of trapping, accumulate changes into the filename stored
  // in EVRP_TRAPS.
  if (const char *filename = getenv ("EVRP_TRAPS"))
    {
      out = fopen (filename, "a");
      accumulate_changes = true;
    }
}

gimple_state::~gimple_state ()
{
  if (out != stderr)
    fclose (out);
}

gimple *
gimple_state::set_orig_stmt (gimple *stmt)
{
  untainted_stmt = stmt;
  gimple *prev = orig_stmt;
  orig_stmt = gimple_copy (stmt);
  return prev;
}

void
gimple_state::maybe_dump_differences_and_trap (gimple *new_stmt, tree lhs)
{
  if (flag_evrp_traps)
    {
      if (unseen_function_p ())
	{
	  fprintf (out, "===============================================\n");
	  highlighter.set (untainted_stmt, orig_stmt, new_stmt, lhs);
	  dump_function_to_file (current_function_decl, out, TDF_NONE);
	  highlighter.set ();
	}
      if (accumulate_changes)
	return;
      gcc_unreachable ();
    }
}

//====================================================
// Miscellaneous debugging aids.
//====================================================

DEBUG_FUNCTION char *
strcpy_gimple (char *dest, const gimple *g)
{
  pretty_printer pp;
  pp_gimple_stmt_1 (&pp, g, 0, TDF_NONE);
  const char *str = pp_formatted_text (&pp);
  return strcpy (dest, str);
}

DEBUG_FUNCTION int
strncmp_gimple (const gimple *g, const char *str, size_t n)
{
  char gstr[100];
  strcpy_gimple (gstr, g);
  return strncmp (gstr, str, n);
}

DEBUG_FUNCTION int
strcmp_gimple (const gimple *g, const char *str)
{
  char gstr[100];
  strcpy_gimple (gstr, g);
  return strcmp (gstr, str);
}

DEBUG_FUNCTION char *
strcpy_tree (char *dest, tree t)
{
  pretty_printer pp;
  dump_generic_node (&pp, t, 0, TDF_VOPS|TDF_MEMSYMS, false);
  const char *str = pp_formatted_text (&pp);
  return strcpy (dest, str);
}

DEBUG_FUNCTION int
strncmp_tree (tree t, const char *str, size_t n)
{
  char tstr[100];
  strcpy_tree (tstr, t);
  return strncmp (tstr, str, n);
}

DEBUG_FUNCTION int
strcmp_tree (tree t, const char *str)
{
  char tstr[100];
  strcpy_tree (tstr, t);
  return strcmp (tstr, str);
}

//=========================================
// vr_comparison
//=========================================

vr_comparison::vr_comparison (const irange *old_range,
			      const irange *new_range,
			      vr_values *vr)
{
  m_old_range = old_range;
  m_new_range = new_range;
  m_vr_values = vr;
}

void
vr_comparison::compare ()
{
  if (new_range_is_same ())
    return;
  if (new_range_is_better ())
    {
      if (dump_file)
	dump_improvements (dump_file);
      return;
    }
  dump_differences_and_trap ();
}

void
vr_comparison::compare (tree name, edge e)
{
  m_name = name;
  m_edge = e;
  m_stmt = NULL;
  compare ();
}

void
vr_comparison::compare (gimple *stmt)
{
  m_name = get_output_for_vrp (stmt);
  m_edge = NULL;
  m_stmt = stmt;
  compare ();
}

bool
vr_comparison::new_range_is_same () const
{
  // We may be able to normalize a symbolic to a [MIN,MAX] plus
  // or minus the end-points.  Don't count that as a win just yet.
  if (m_old_range && m_old_range->symbolic_p ())
    return true;
  widest_irange old;
  if (m_old_range)
    old = *m_old_range;
  // Treat UNDEFINED and VARYING as interchangeable.
  if (old.undefined_p () && m_new_range->varying_p ())
    return true;
  if (old.varying_p () && m_new_range->undefined_p ())
    return true;
  return old == *m_new_range;
}

bool
vr_comparison::new_range_is_better () const
{
  if (new_range_is_same ())
    return false;
  if (!m_old_range)
    return true;
  // ?? Sometimes we get an undefined because the ranger determined a
  // path was unexecutable.  Verify this is actually the case by
  // turning this off and analyzing all failures.
  if (m_new_range->undefined_p ())
    return true;
  if (!range_has_numeric_bounds_p (m_old_range))
    {
      gcc_checking_assert (range_has_numeric_bounds_p (m_new_range));
      return true;
    }
  widest_irange inter (*m_new_range);
  inter.intersect (*m_old_range);
  return inter == *m_new_range;
}

void
vr_comparison::dump_differences_and_trap () const
{
  bool dumping = getenv("GORI_DUMP_FILE") != NULL;
  if (dumping)
    {
      const char *filename = getenv("GORI_DUMP_FILE");
      FILE *out = fopen (filename, "a");
      fprintf (out, "=========FILE: %s ========\n",
	       main_input_filename ? main_input_filename : "UNKNOWN");
      dump_differences (out);
      fclose (out);
      return;
    }
  dump_differences (stderr);
  gcc_unreachable ();
}

void
vr_comparison::dump_differences (FILE *out) const
{
  dump_flags_t flags = TDF_NONE;
  if (m_stmt)
    {
      fprintf (out, "Different ranges for stmt: ");
      print_gimple_stmt (out, m_stmt, 0, flags);
    }
  else
    {
      fprintf (out, "Different ranges on edge (%d -> %d) for SSA: ",
	       m_edge->src->index, m_edge->dest->index);
      print_generic_stmt (out, m_name, flags);
    }
  fprintf (out, "\told range: ");
  m_old_range->dump (out);
  fprintf (out, "\n\tnew range: ");
  m_new_range->dump (out);
  if (m_edge)
    {
      fprintf (out, "\n\n");
      dump_bb (out, m_edge->src, 0, TDF_NONE);
    }
  fprintf (out, "\n");
  fprintf (out, "==============================================\n");
  dump_function_to_file (current_function_decl, out, TDF_NONE);

  if (m_vr_values)
    {
      dump_flags_t save = dump_flags;
      dump_flags |= TDF_GORI;
      m_vr_values->dump_all_value_ranges (out);
      dump_flags = save;
    }
}

void
vr_comparison::dump_improvements (FILE *out) const
{
  if (m_old_range && !range_has_numeric_bounds_p (m_old_range))
    return;
  if (new_range_is_better ())
    {
      fprintf (out, "New range improved: ");
      if (m_name)
	print_generic_expr (out, m_name);
      fprintf (out, " from: ");
      if (m_old_range)
	m_old_range->dump (out);
      else
	fprintf (out, "UNDEFINED");
      fprintf (out, " to: ");
      m_new_range->dump (out);
      fprintf (out, "\n");
    }
}
