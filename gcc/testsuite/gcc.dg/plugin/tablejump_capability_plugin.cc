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
#include "tree-pass.h"
#include "target.h"
#include "context.h"
#include "plugin-version.h"

int plugin_is_GPL_compatible;

static bool (*saved_can_share) (rtx_insn *);
static bool (*saved_cannot_copy) (rtx_insn *);
static void (*saved_base) (FILE *, rtx_jump_table_data *, unsigned long);
static unsigned queries, late_queries, veto_queries;
static unsigned functions, expected_bases, emitted_bases;
static bool veto;

/* Return whether the current function enables CRC instructions.  */

static bool
crc_enabled_p ()
{
#ifdef AARCH64_HAVE_ISA
  return TARGET_CRC32;
#else
  gcc_unreachable ();
#endif
}

/* Check the real tablejump INSN and allow sharing according to the current
   function's target options.  The target's own answer does not depend on
   whether it emits a table base.  */

static bool
can_share (rtx_insn *insn)
{
  gcc_assert (tablejump_p (insn, NULL, NULL));
  auto base = targetm.asm_out.jump_table_base;
  targetm.asm_out.jump_table_base = NULL;
  gcc_assert (saved_can_share (insn));
  targetm.asm_out.jump_table_base = base;
  gcc_assert (saved_can_share (insn));
  ++queries;
  return crc_enabled_p ();
}

/* Reject sharing and record any query outside the duplication pass.  */

static bool
reject_late (rtx_insn *)
{
  ++late_queries;
  return false;
}

/* Reject copying a tablejump INSN in the selected function.  */

static bool
cannot_copy (rtx_insn *insn)
{
  if (veto && tablejump_p (insn, NULL, NULL))
    {
      ++veto_queries;
      return true;
    }
  return saved_cannot_copy && saved_cannot_copy (insn);
}

/* Emit TABLE's base with LABELNO to STREAM while eligibility is false.  */

static void
emit_base (FILE *stream, rtx_jump_table_data *table, unsigned long labelno)
{
  gcc_assert (targetm.can_share_jump_table_p == reject_late);
  gcc_assert (JUMP_TABLE_SHARED_P (PATTERN (table)));
  ++emitted_bases;
  if (saved_base)
    saved_base (stream, table, labelno);
}

const pass_data capability_pass_data =
{
  RTL_PASS, "tablejump_capability", OPTGROUP_NONE, TV_NONE,
  PROP_rtl, 0, 0, 0, 0
};

class capability_pass : public rtl_opt_pass
{
public:
  capability_pass (gcc::context *ctxt, bool before)
    : rtl_opt_pass (capability_pass_data, ctxt), m_before (before)
  {}

  unsigned int execute (function *fun) final override
  {
    const char *name = IDENTIFIER_POINTER (DECL_NAME (fun->decl));
    bool ordinary = !strcmp (name, "ordinary");
    bool allowed = !strcmp (name, "allowed");
    unsigned tables = 0, shared = 0, jumps = 0;
    for (rtx_insn *insn = get_insns (); insn; insn = NEXT_INSN (insn))
      {
	if (JUMP_TABLE_DATA_P (insn))
	  {
	    ++tables;
	    shared += JUMP_TABLE_SHARED_P (PATTERN (insn));
	  }
	jumps += tablejump_p (insn, NULL, NULL);
      }
    gcc_assert (tables == (ordinary ? 0 : 1));
    gcc_assert (!late_queries);
    if (!ordinary)
      gcc_assert (crc_enabled_p () == !!strcmp (name, "rejected"));

    if (m_before)
      {
	gcc_assert (!shared && jumps == tables);
	queries = veto_queries = 0;
	veto = !strcmp (name, "vetoed");
	targetm.can_share_jump_table_p = can_share;
	targetm.cannot_copy_insn_p = cannot_copy;
      }
    else
      {
	gcc_assert ((queries != 0) == !ordinary);
	gcc_assert (shared == (allowed ? 1 : 0));
	gcc_assert (allowed ? jumps > tables : jumps == tables);
	gcc_assert ((veto_queries != 0) == veto);
	expected_bases += shared;
	++functions;
	targetm.can_share_jump_table_p = reject_late;
	targetm.cannot_copy_insn_p = saved_cannot_copy;
      }
    return 0;
  }

private:
  bool m_before;
};

/* Check that all driver functions were tested and all shared bases emitted.  */

static void
finish_unit (void *, void *)
{
  gcc_assert (functions == 4);
  gcc_assert (expected_bases == 1 && emitted_bases == expected_bases);
  gcc_assert (!late_queries);
}

int
plugin_init (struct plugin_name_args *info, struct plugin_gcc_version *version)
{
  if (!plugin_default_version_check (version, &gcc_version))
    return 1;
  saved_can_share = targetm.can_share_jump_table_p;
  saved_cannot_copy = targetm.cannot_copy_insn_p;
  saved_base = targetm.asm_out.jump_table_base;
  targetm.can_share_jump_table_p = reject_late;
  targetm.asm_out.jump_table_base = emit_base;
  for (unsigned i = 0; i != 2; ++i)
    {
      struct register_pass_info pass;
      pass.pass = new capability_pass (g, i == 0);
      pass.reference_pass_name = "compgotos";
      pass.ref_pass_instance_number = 1;
      pass.pos_op = i == 0 ? PASS_POS_INSERT_BEFORE : PASS_POS_INSERT_AFTER;
      register_callback (info->base_name, PLUGIN_PASS_MANAGER_SETUP, NULL,
			 &pass);
    }
  register_callback (info->base_name, PLUGIN_FINISH_UNIT, finish_unit, NULL);
  return 0;
}
