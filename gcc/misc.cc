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
// enhanced_operand_compare
//====================================================

class enhanced_operand_compare : public operand_compare
{
public:
  bool operand_equal_p (const_tree, const_tree, unsigned int) OVERRIDE;
private:
  bool compare_operands (const_tree op1, const_tree op2, unsigned n);
};

bool
enhanced_operand_compare::compare_operands (const_tree op1, const_tree op2,
					    unsigned n)
{
  for (unsigned i = 0; i < n; ++i)
    if (!operand_equal_p (TREE_OPERAND (op1, i), TREE_OPERAND (op2, i),
			  OEP_MATCH_SIDE_EFFECTS))
      return false;
  return true;
}

bool
enhanced_operand_compare::operand_equal_p (const_tree op1, const_tree op2,
					   unsigned int flags)
{
  bool r;
  if (verify_hash_value (op1, op2, flags, &r))
    return r;

  if (op1 == op2)
    return true;

  if (!op1 || !op2)
    return op1 == op2;

  if (TREE_CODE (op1) != TREE_CODE (op2))
    return false;

  flags = OEP_MATCH_SIDE_EFFECTS | OEP_NO_HASH_CHECK;
  switch (TREE_CODE (op1))
    {
    case CASE_LABEL_EXPR:
      return compare_operands (op1, op2, 4);

      // There's a bunch of expressions that operand_equal_p do not
      // handle.  Assume those are equal.
    case CONSTRUCTOR:
    case WITH_SIZE_EXPR:
    case OBJ_TYPE_REF:
      return true;

    case TREE_LIST:
      return (operand_equal_p (TREE_PURPOSE (op1), TREE_PURPOSE (op2), flags)
	      && operand_equal_p (TREE_VALUE (op1), TREE_VALUE (op2), flags));
    default:
      return operand_compare::operand_equal_p (op1, op2, flags);
    }
}

//====================================================
// gimple_different_p
//====================================================

static bool
gimple_phi_different_p (gphi *orig_phi, gphi *new_phi)
{
  gcc_checking_assert (gimple_phi_num_args (orig_phi)
		       == gimple_phi_num_args (new_phi));
  for (unsigned i = 0; i < gimple_phi_num_args (orig_phi); ++i)
    {
      phi_arg_d *orig_arg = gimple_phi_arg (orig_phi, i);
      phi_arg_d *new_arg = gimple_phi_arg (new_phi, i);
      if (memcmp (orig_arg, new_arg, sizeof (phi_arg_d)))
	return true;
    }
  return false;
}

static bool
gimple_different_p (gimple *orig_stmt, gimple *new_stmt)
{
  if (gimple_code (orig_stmt) != gimple_code (new_stmt)
      || orig_stmt->subcode != new_stmt->subcode)
    return true;

  if (gimple_num_ops (orig_stmt) != gimple_num_ops (new_stmt))
    return true;

  switch (gimple_code (new_stmt))
    {
    case GIMPLE_PHI:
      return gimple_phi_different_p (as_a <gphi *> (orig_stmt),
				     as_a <gphi *> (new_stmt));
    case GIMPLE_ASM:
      return false;
    default:
      break;
    }

  enhanced_operand_compare comparator;
  for (unsigned i = 0; i < gimple_num_ops (orig_stmt); ++i)
    {
      tree op1 = gimple_op (orig_stmt, i);
      tree op2 = gimple_op (new_stmt, i);
      if (!comparator.operand_equal_p (op1, op2, 0))
	return true;
    }
  return false;
}

//====================================================
// gimple_state
//====================================================

gimple_state::gimple_state ()
{
  orig_stmt = NULL;
  if (const char *filename = getenv ("GIMPLE_CHANGES"))
    {
      out = fopen (filename, "a");
      trap = false;
    }
  else
    {
      out = stderr;
      trap = true;
    }
}

gimple_state::~gimple_state ()
{
  if (out != stderr)
    fclose (out);
}

gimple *
gimple_state::save (gimple *stmt)
{
  untainted_stmt = stmt;
  gimple *prev = orig_stmt;
  orig_stmt = gimple_copy (stmt);
  return prev;
}

void
gimple_state::trap_if_gimple_changed (gimple *new_stmt)
{
  if (is_gimple_debug (new_stmt))
    return;
  if (gimple_different_p (orig_stmt, new_stmt))
    {
      dump_differences (out, new_stmt);
      if (trap)
	gcc_unreachable ();
    }
}

void
gimple_state::dump_differences (FILE *out, gimple *new_stmt, tree lhs)
{
  if (unseen_function_p ())
    {
      fprintf (out, "===================================================\n");
      highlighter.set (untainted_stmt, orig_stmt, new_stmt, lhs);
      dump_function_to_file (current_function_decl, out, TDF_NONE);
      highlighter.set ();
    }
}

void
gimple_state::remove (gimple *stmt, tree lhs)
{
  // Set orig_stmt because removing PHIs seems to change the original
  // statement.
  gimple *prev = save (stmt);
  dump_differences (out, NULL, lhs);
  save (prev);
  if (trap)
    gcc_unreachable ();
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
