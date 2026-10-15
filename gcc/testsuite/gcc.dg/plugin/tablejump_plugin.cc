/* PR middle-end/126236 */
/* { dg-options "-O0" } */

#include "gcc-plugin.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "function.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "basic-block.h"
#include "cfg.h"
#include "cfghooks.h"
#include "cfgrtl.h"
#include "tree-pass.h"
#include "target.h"
#include "context.h"
#include "plugin-version.h"

int plugin_is_GPL_compatible;

static rtx_insn *uncopyable_jump;

/* Return true if INSN is the selected target-specific table user.  */

static bool
cannot_copy_jump (rtx_insn *insn)
{
  return insn == uncopyable_jump;
}

/* Check sticky shared flags with zero, one and two users, including repeated
   table entries.  Keep this synthetic RTL out of the output pipeline.  */

static void
check_user_counts ()
{
  start_sequence ();
  rtx_code_label *dest = gen_label_rtx ();
  emit_label (dest);
  rtx_code_label *label = gen_label_rtx ();
  emit_label (label);
  rtvec entries = gen_rtvec (2, gen_rtx_LABEL_REF (Pmode, dest),
			   gen_rtx_LABEL_REF (Pmode, dest));
  rtx_jump_table_data *table
    = emit_jump_table_data (gen_rtx_ADDR_DIFF_VEC
			   (SImode, gen_rtx_LABEL_REF (Pmode, label),
			    entries, const0_rtx, const0_rtx));
  JUMP_TABLE_SHARED_P (PATTERN (table)) = 1;
  verify_jump_tables ();
  for (int i = 0; i != 2; ++i)
    {
      rtx pattern = gen_rtx_PARALLEL
	(VOIDmode, gen_rtvec
	 (2, gen_rtx_SET (pc_rtx, gen_rtx_REG (Pmode, 0)),
	  gen_rtx_USE (VOIDmode, gen_rtx_LABEL_REF (Pmode, label))));
      rtx_insn *jump = emit_jump_insn (pattern);
      JUMP_LABEL (jump) = label;
      verify_jump_tables ();
    }
  auto saved_cannot_copy = targetm.cannot_copy_insn_p;
  uncopyable_jump = get_last_insn ();
  targetm.cannot_copy_insn_p = cannot_copy_jump;
  JUMP_TABLE_SHARED_P (PATTERN (table)) = 0;
  verify_jump_tables ();
  JUMP_TABLE_SHARED_P (PATTERN (table)) = 1;
  targetm.cannot_copy_insn_p = saved_cannot_copy;
  uncopyable_jump = NULL;
  PUT_MODE (PATTERN (table), QImode);
  verify_jump_tables ();
  rtx_insn *jump = get_last_insn ();
  remove_insn (jump);
  SET_PREV_INSN (jump) = SET_NEXT_INSN (jump) = NULL;
  add_insn (make_insn_raw (gen_rtx_SEQUENCE
			 (VOIDmode, gen_rtvec (1, jump))));
  verify_jump_tables ();
  end_sequence ();
}

/* Return a block that is not a successor of BB.  */

static basic_block
other_block (basic_block bb)
{
  basic_block dest;
  FOR_EACH_BB_FN (dest, cfun)
    if (dest != bb && !find_edge (bb, dest))
      return dest;
  gcc_unreachable ();
}

/* Return a block that does not end in a jump through the table whose label
   is LABEL.  */

static basic_block
non_user_block (rtx_insn *label)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    if (!JUMP_P (BB_END (bb)) || JUMP_LABEL (BB_END (bb)) != label)
      return bb;
  gcc_unreachable ();
}

/* Check a CASESI default edge and abnormal edges for JUMP.  Restore its
   pattern and edges before target-specific processing resumes.  */

static void
check_successors (rtx_insn *jump)
{
  basic_block bb = BLOCK_FOR_INSN (jump);
  rtx_code_label *dest = block_label (other_block (bb));
  edge extra = make_edge (bb, BLOCK_FOR_INSN (dest), 0);
  extra->probability = profile_probability::never ();
  rtx saved_pattern = PATTERN (jump);
  int saved_code = INSN_CODE (jump);
  PATTERN (jump) = gen_rtx_PARALLEL
    (VOIDmode, gen_rtvec
     (2, gen_rtx_SET
      (pc_rtx, gen_rtx_IF_THEN_ELSE
       (Pmode, gen_rtx_EQ (VOIDmode, gen_rtx_REG (Pmode, 0), const0_rtx),
	gen_rtx_REG (Pmode, 0), gen_rtx_LABEL_REF (Pmode, dest))),
      gen_rtx_USE (VOIDmode, gen_rtx_LABEL_REF (Pmode, JUMP_LABEL (jump)))));
  INSN_CODE (jump) = -1;
  verify_flow_info ();
  PATTERN (jump) = saved_pattern;
  INSN_CODE (jump) = saved_code;
  extra->flags = EDGE_ABNORMAL;
  verify_flow_info ();
  remove_edge (extra);

  edge existing = EDGE_SUCC (bb, 0);
  int saved_flags = existing->flags;
  existing->flags |= EDGE_ABNORMAL;
  verify_flow_info ();
  existing->flags = saved_flags;
}

const pass_data tablejump_pass_data =
{
  RTL_PASS, "tablejump_test", OPTGROUP_NONE, TV_NONE,
  PROP_rtl, 0, 0, 0, 0
};

class tablejump_pass : public rtl_opt_pass
{
public:
  tablejump_pass (gcc::context *ctxt, bool cfg)
    : rtl_opt_pass (tablejump_pass_data, ctxt), m_cfg (cfg)
  {}

  unsigned int execute (function *fun) final override
  {
    const char *name = IDENTIFIER_POINTER (DECL_NAME (fun->decl));
    bool cfg_test = (!strcmp (name, "missing_edge")
		     || !strcmp (name, "extra_edge")
		     || !strcmp (name, "misplaced_table"));
    bool valid = !strcmp (name, "valid");
    if (!valid && m_cfg != cfg_test)
      return 0;

    rtx_insn *jump = NULL;
    rtx_insn *label = NULL;
    rtx_jump_table_data *table = NULL;
    for (rtx_insn *insn = get_insns (); insn; insn = NEXT_INSN (insn))
      if (tablejump_p (insn, &label, &table))
	{
	  jump = insn;
	  break;
	}
    gcc_assert (jump && JUMP_TABLE_SHARED_P (PATTERN (table)));

    if (valid)
      {
	if (m_cfg)
	  check_successors (jump);
	else
	  check_user_counts ();
	return 0;
      }

    if (!strcmp (name, "missing_edge"))
      remove_edge (EDGE_SUCC (BLOCK_FOR_INSN (jump), 0));
    else if (!strcmp (name, "extra_edge"))
      {
	basic_block bb = BLOCK_FOR_INSN (jump);
	edge extra = make_edge (bb, other_block (bb), 0);
	extra->probability = profile_probability::never ();
      }
    else if (!strcmp (name, "null_jump_label"))
      JUMP_LABEL (jump) = NULL_RTX;
    else if (!strcmp (name, "stale_jump_label"))
      JUMP_LABEL (jump) = gen_label_rtx ();
    else if (!strcmp (name, "missing_pattern_ref"))
      PATTERN (jump) = gen_rtx_SET (pc_rtx, gen_rtx_REG (Pmode, 0));
    else if (!strcmp (name, "misplaced_table"))
      reorder_insns_nobb (label, table, BB_END (non_user_block (label)));
    else if (!strcmp (name, "detached_label"))
      remove_insn (label);
    else if (!strcmp (name, "detached_table"))
      remove_insn (table);
    else if (!strcmp (name, "detached_pair"))
      {
	remove_insn (table);
	remove_insn (label);
      }
    else if (!strcmp (name, "deleted_table"))
      delete_insn (table);
    else if (!strcmp (name, "unmarked_table"))
      JUMP_TABLE_SHARED_P (PATTERN (table)) = 0;
    else if (!strcmp (name, "missing_case_label"))
      XEXP (RTVEC_ELT (table->get_labels (), 0), 0) = gen_label_rtx ();
    else if (!strcmp (name, "missing_base_label"))
      {
	gcc_assert (GET_CODE (PATTERN (table)) == ADDR_DIFF_VEC);
	XEXP (XEXP (PATTERN (table), 0), 0) = gen_label_rtx ();
      }
    else
      gcc_unreachable ();
    return 0;
  }

private:
  bool m_cfg;
};

int
plugin_init (struct plugin_name_args *info, struct plugin_gcc_version *version)
{
  if (!plugin_default_version_check (version, &gcc_version))
    return 1;
  const char *points[] = { "compgotos", "*free_cfg" };
  for (unsigned i = 0; i != ARRAY_SIZE (points); ++i)
    {
      struct register_pass_info pass;
      pass.pass = new tablejump_pass (g, i == 0);
      pass.reference_pass_name = points[i];
      pass.ref_pass_instance_number = 1;
      pass.pos_op = PASS_POS_INSERT_AFTER;
      register_callback (info->base_name, PLUGIN_PASS_MANAGER_SETUP, NULL,
			 &pass);
    }
  return 0;
}
