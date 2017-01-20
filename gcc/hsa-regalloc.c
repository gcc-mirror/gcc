/* HSAIL IL Register allocation and out-of-SSA.
   Copyright (C) 2013-2017 Free Software Foundation, Inc.
   Contributed by Michael Matz <matz@suse.de>

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
#include "tm.h"
#include "is-a.h"
#include "vec.h"
#include "tree.h"
#include "dominance.h"
#include "cfg.h"
#include "cfganal.h"
#include "function.h"
#include "bitmap.h"
#include "dumpfile.h"
#include "cgraph.h"
#include "print-tree.h"
#include "cfghooks.h"
#include "symbol-summary.h"
#include "hsa-common.h"


/* Process a PHI node PHI of basic block BB as a part of naive out-f-ssa.  */

static void
naive_process_phi (hsa_insn_phi *phi)
{
  unsigned count = phi->operand_count ();
  for (unsigned i = 0; i < count; i++)
    {
      gcc_checking_assert (phi->get_op (i));
      hsa_op_base *op = phi->get_op (i);
      hsa_bb *hbb;
      edge e;

      if (!op)
	break;

      e = EDGE_PRED (phi->m_bb, i);
      if (single_succ_p (e->src))
	hbb = hsa_bb_for_bb (e->src);
      else
	{
	  basic_block old_dest = e->dest;
	  hbb = hsa_init_new_bb (split_edge (e));

	  /* If switch insn used this edge, fix jump table.  */
	  hsa_bb *source = hsa_bb_for_bb (e->src);
	  hsa_insn_sbr *sbr;
	  if (source->m_last_insn
	      && (sbr = dyn_cast <hsa_insn_sbr *> (source->m_last_insn)))
	    sbr->replace_all_labels (old_dest, hbb->m_bb);
	}

      hsa_build_append_simple_mov (phi->m_dest, op, hbb);
    }
}

/* Naive out-of SSA.  */

static void
naive_outof_ssa (void)
{
  basic_block bb;

  hsa_cfun->m_in_ssa = false;

  FOR_ALL_BB_FN (bb, cfun)
  {
    hsa_bb *hbb = hsa_bb_for_bb (bb);
    hsa_insn_phi *phi;

    for (phi = hbb->m_first_phi;
	 phi;
	 phi = phi->m_next ? as_a <hsa_insn_phi *> (phi->m_next) : NULL)
      naive_process_phi (phi);

    /* Zap PHI nodes, they will be deallocated when everything else will.  */
    hbb->m_first_phi = NULL;
    hbb->m_last_phi = NULL;
  }
}

/* Return register class number for the given HSA TYPE.  0 means the 'c' one
   bit register class, 1 means 's' 32 bit class, 2 stands for 'd' 64 bit class
   and 3 for 'q' 128 bit class.  */

static int
m_reg_class_for_type (BrigType16_t type)
{
  switch (type)
    {
    case BRIG_TYPE_B1:
      return 0;

    case BRIG_TYPE_U8:
    case BRIG_TYPE_U16:
    case BRIG_TYPE_U32:
    case BRIG_TYPE_S8:
    case BRIG_TYPE_S16:
    case BRIG_TYPE_S32:
    case BRIG_TYPE_F16:
    case BRIG_TYPE_F32:
    case BRIG_TYPE_B8:
    case BRIG_TYPE_B16:
    case BRIG_TYPE_B32:
    case BRIG_TYPE_U8X4:
    case BRIG_TYPE_S8X4:
    case BRIG_TYPE_U16X2:
    case BRIG_TYPE_S16X2:
    case BRIG_TYPE_F16X2:
      return 1;

    case BRIG_TYPE_U64:
    case BRIG_TYPE_S64:
    case BRIG_TYPE_F64:
    case BRIG_TYPE_B64:
    case BRIG_TYPE_U8X8:
    case BRIG_TYPE_S8X8:
    case BRIG_TYPE_U16X4:
    case BRIG_TYPE_S16X4:
    case BRIG_TYPE_F16X4:
    case BRIG_TYPE_U32X2:
    case BRIG_TYPE_S32X2:
    case BRIG_TYPE_F32X2:
      return 2;

    case BRIG_TYPE_B128:
    case BRIG_TYPE_U8X16:
    case BRIG_TYPE_S8X16:
    case BRIG_TYPE_U16X8:
    case BRIG_TYPE_S16X8:
    case BRIG_TYPE_F16X8:
    case BRIG_TYPE_U32X4:
    case BRIG_TYPE_U64X2:
    case BRIG_TYPE_S32X4:
    case BRIG_TYPE_S64X2:
    case BRIG_TYPE_F32X4:
    case BRIG_TYPE_F64X2:
      return 3;

    default:
      gcc_unreachable ();
    }
}

/* If the Ith operands of INSN is or contains a register (in an address),
   return the address of that register operand.  If not return NULL.  */

static hsa_op_reg **
insn_reg_addr (hsa_insn_basic *insn, int i)
{
  hsa_op_base *op = insn->get_op (i);
  if (!op)
    return NULL;
  hsa_op_reg *reg = dyn_cast <hsa_op_reg *> (op);
  if (reg)
    return (hsa_op_reg **) insn->get_op_addr (i);
  hsa_op_address *addr = dyn_cast <hsa_op_address *> (op);
  if (addr && addr->m_reg)
    return &addr->m_reg;
  return NULL;
}

struct m_reg_class_desc
{
  unsigned next_avail, max_num;
  unsigned used_num, max_used;
  uint64_t used[2];
  char cl_char;
};

/* Rewrite the instructions in BB to observe spilled live ranges.
   CLASSES is the global register class state.  */

static void
rewrite_code_bb (basic_block bb, struct m_reg_class_desc *classes)
{
  hsa_bb *hbb = hsa_bb_for_bb (bb);
  hsa_insn_basic *insn, *next_insn;

  for (insn = hbb->m_first_insn; insn; insn = next_insn)
    {
      next_insn = insn->m_next;
      unsigned count = insn->operand_count ();
      for (unsigned i = 0; i < count; i++)
	{
	  gcc_checking_assert (insn->get_op (i));
	  hsa_op_reg **regaddr = insn_reg_addr (insn, i);

	  if (regaddr)
	    {
	      hsa_op_reg *reg = *regaddr;
	      if (reg->m_reg_class)
		continue;
	      gcc_assert (reg->m_spill_sym);

	      int cl = m_reg_class_for_type (reg->m_type);
	      hsa_op_reg *tmp, *tmp2;
	      if (insn->op_output_p (i))
		tmp = hsa_spill_out (insn, reg, &tmp2);
	      else
		tmp = hsa_spill_in (insn, reg, &tmp2);

	      *regaddr = tmp;

	      tmp->m_reg_class = classes[cl].cl_char;
	      tmp->m_hard_num = (char) (classes[cl].max_num + i);
	      if (tmp2)
		{
		  gcc_assert (cl == 0);
		  tmp2->m_reg_class = classes[1].cl_char;
		  tmp2->m_hard_num = (char) (classes[1].max_num + i);
		}
	    }
	}
    }
}

/* Dump current function to dump file F, with info specific
   to register allocation.  */

void
dump_hsa_cfun_regalloc (FILE *f)
{
  basic_block bb;

  fprintf (f, "\nHSAIL IL for %s\n", hsa_cfun->m_name);

  FOR_ALL_BB_FN (bb, cfun)
  {
    hsa_bb *hbb = (struct hsa_bb *) bb->aux;
    bitmap_print (dump_file, hbb->m_livein, "m_livein  ", "\n");
    dump_hsa_bb (f, hbb);
    bitmap_print (dump_file, hbb->m_liveout, "m_liveout ", "\n");
  }
}

/* Given the global register allocation state CLASSES and a
   register REG, try to give it a hardware register.  If successful,
   store that hardreg in REG and return it, otherwise return -1.
   Also changes CLASSES to accommodate for the allocated register.  */

static int
try_alloc_reg (struct m_reg_class_desc *classes, hsa_op_reg *reg)
{
  int cl = m_reg_class_for_type (reg->m_type);
  int ret = -1;
  if (classes[1].used_num + classes[2].used_num * 2 + classes[3].used_num * 4
      >= 128 - 5)
    return -1;
  if (classes[cl].used_num < classes[cl].max_num)
    {
      unsigned int i;
      classes[cl].used_num++;
      if (classes[cl].used_num > classes[cl].max_used)
	classes[cl].max_used = classes[cl].used_num;
      for (i = 0; i < classes[cl].used_num; i++)
	if (! (classes[cl].used[i / 64] & (((uint64_t)1) << (i & 63))))
	  break;
      ret = i;
      classes[cl].used[i / 64] |= (((uint64_t)1) << (i & 63));
      reg->m_reg_class = classes[cl].cl_char;
      reg->m_hard_num = i;
    }
  return ret;
}

/* Free up hardregs used by REG, into allocation state CLASSES.  */

static void
free_reg (struct m_reg_class_desc *classes, hsa_op_reg *reg)
{
  int cl = m_reg_class_for_type (reg->m_type);
  int ret = reg->m_hard_num;
  gcc_assert (reg->m_reg_class == classes[cl].cl_char);
  classes[cl].used_num--;
  classes[cl].used[ret / 64] &= ~(((uint64_t)1) << (ret & 63));
}

/* Note that the live range for REG ends at least at END.  */

static void
note_lr_end (hsa_op_reg *reg, int end)
{
  if (reg->m_lr_end < end)
    reg->m_lr_end = end;
}

/* Note that the live range for REG starts at least at BEGIN.  */

static void
note_lr_begin (hsa_op_reg *reg, int begin)
{
  if (reg->m_lr_begin > begin)
    reg->m_lr_begin = begin;
}

/* Given two registers A and B, return -1, 0 or 1 if A's live range
   starts before, at or after B's live range.  */

static int
cmp_begin (const void *a, const void *b)
{
  const hsa_op_reg * const *rega = (const hsa_op_reg * const *)a;
  const hsa_op_reg * const *regb = (const hsa_op_reg * const *)b;
  int ret;
  if (rega == regb)
    return 0;
  ret = (*rega)->m_lr_begin - (*regb)->m_lr_begin;
  if (ret)
    return ret;
  return ((*rega)->m_order - (*regb)->m_order);
}

/* Given two registers REGA and REGB, return true if REGA's
   live range ends after REGB's.  This results in a sorting order
   with earlier end points at the end.  */

static bool
cmp_end (hsa_op_reg * const &rega, hsa_op_reg * const &regb)
{
  int ret;
  if (rega == regb)
    return false;
  ret = (regb)->m_lr_end - (rega)->m_lr_end;
  if (ret)
    return ret < 0;
  return (((regb)->m_order - (rega)->m_order)) < 0;
}

/* Expire all old intervals in ACTIVE (a per-regclass vector),
   that is, those that end before the interval REG starts.  Give
   back resources freed so into the state CLASSES.  */

static void
expire_old_intervals (hsa_op_reg *reg, vec<hsa_op_reg*> *active,
		      struct m_reg_class_desc *classes)
{
  for (int i = 0; i < 4; i++)
    while (!active[i].is_empty ())
      {
	hsa_op_reg *a = active[i].pop ();
	if (a->m_lr_end > reg->m_lr_begin)
	  {
	    active[i].quick_push (a);
	    break;
	  }
	free_reg (classes, a);
      }
}

/* The interval REG didn't get a hardreg.  Spill it or one of those
   from ACTIVE (if the latter, then REG will become allocated to the
   hardreg that formerly was used by it).  */

static void
spill_at_interval (hsa_op_reg *reg, vec<hsa_op_reg*> *active)
{
  int cl = m_reg_class_for_type (reg->m_type);
  gcc_assert (!active[cl].is_empty ());
  hsa_op_reg *cand = active[cl][0];
  if (cand->m_lr_end > reg->m_lr_end)
    {
      reg->m_reg_class = cand->m_reg_class;
      reg->m_hard_num = cand->m_hard_num;
      active[cl].ordered_remove (0);
      unsigned place = active[cl].lower_bound (reg, cmp_end);
      active[cl].quick_insert (place, reg);
    }
  else
    cand = reg;

  gcc_assert (!cand->m_spill_sym);
  BrigType16_t type = cand->m_type;
  if (type == BRIG_TYPE_B1)
    type = BRIG_TYPE_U8;
  cand->m_reg_class = 0;
  cand->m_spill_sym = hsa_get_spill_symbol (type);
  cand->m_spill_sym->m_name_number = cand->m_order;
}

/* Given the global register state CLASSES allocate all HSA virtual
   registers either to hardregs or to a spill symbol.  */

static void
linear_scan_regalloc (struct m_reg_class_desc *classes)
{
  /* Compute liveness.  */
  bool changed;
  int i, n;
  int insn_order;
  int *bbs = XNEWVEC (int, n_basic_blocks_for_fn (cfun));
  bitmap work = BITMAP_ALLOC (NULL);
  vec<hsa_op_reg*> ind2reg = vNULL;
  vec<hsa_op_reg*> active[4] = {vNULL, vNULL, vNULL, vNULL};
  hsa_insn_basic *m_last_insn;

  /* We will need the reverse post order for linearization,
     and the post order for liveness analysis, which is the same
     backward.  */
  n = pre_and_rev_post_order_compute (NULL, bbs, true);
  ind2reg.safe_grow_cleared (hsa_cfun->m_reg_count);

  /* Give all instructions a linearized number, at the same time
     build a mapping from register index to register.  */
  insn_order = 1;
  for (i = 0; i < n; i++)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, bbs[i]);
      hsa_bb *hbb = hsa_bb_for_bb (bb);
      hsa_insn_basic *insn;
      for (insn = hbb->m_first_insn; insn; insn = insn->m_next)
	{
	  unsigned opi;
	  insn->m_number = insn_order++;
	  for (opi = 0; opi < insn->operand_count (); opi++)
	    {
	      gcc_checking_assert (insn->get_op (opi));
	      hsa_op_reg **regaddr = insn_reg_addr (insn, opi);
	      if (regaddr)
		ind2reg[(*regaddr)->m_order] = *regaddr;
	    }
	}
    }

  /* Initialize all live ranges to [after-end, 0).  */
  for (i = 0; i < hsa_cfun->m_reg_count; i++)
    if (ind2reg[i])
      ind2reg[i]->m_lr_begin = insn_order, ind2reg[i]->m_lr_end = 0;

  /* Classic liveness analysis, as long as something changes:
       m_liveout is union (m_livein of successors)
       m_livein is m_liveout minus defs plus uses.  */
  do
    {
      changed = false;
      for (i = n - 1; i >= 0; i--)
	{
	  edge e;
	  edge_iterator ei;
	  basic_block bb = BASIC_BLOCK_FOR_FN (cfun, bbs[i]);
	  hsa_bb *hbb = hsa_bb_for_bb (bb);

	  /* Union of successors m_livein (or empty if none).  */
	  bool first = true;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
	      {
		hsa_bb *succ = hsa_bb_for_bb (e->dest);
		if (first)
		  {
		    bitmap_copy (work, succ->m_livein);
		    first = false;
		  }
		else
		  bitmap_ior_into (work, succ->m_livein);
	      }
	  if (first)
	    bitmap_clear (work);

	  bitmap_copy (hbb->m_liveout, work);

	  /* Remove defs, include uses in a backward insn walk.  */
	  hsa_insn_basic *insn;
	  for (insn = hbb->m_last_insn; insn; insn = insn->m_prev)
	    {
	      unsigned opi;
	      unsigned ndefs = insn->input_count ();
	      for (opi = 0; opi < ndefs && insn->get_op (opi); opi++)
		{
		  gcc_checking_assert (insn->get_op (opi));
		  hsa_op_reg **regaddr = insn_reg_addr (insn, opi);
		  if (regaddr)
		    bitmap_clear_bit (work, (*regaddr)->m_order);
		}
	      for (; opi < insn->operand_count (); opi++)
		{
		  gcc_checking_assert (insn->get_op (opi));
		  hsa_op_reg **regaddr = insn_reg_addr (insn, opi);
		  if (regaddr)
		    bitmap_set_bit (work, (*regaddr)->m_order);
		}
	    }

	  /* Note if that changed something.  */
	  if (bitmap_ior_into (hbb->m_livein, work))
	    changed = true;
	}
    }
  while (changed);

  /* Make one pass through all instructions in linear order,
     noting and merging possible live range start and end points.  */
  m_last_insn = NULL;
  for (i = n - 1; i >= 0; i--)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, bbs[i]);
      hsa_bb *hbb = hsa_bb_for_bb (bb);
      hsa_insn_basic *insn;
      int after_end_number;
      unsigned bit;
      bitmap_iterator bi;

      if (m_last_insn)
	after_end_number = m_last_insn->m_number;
      else
	after_end_number = insn_order;
      /* Everything live-out in this BB has at least an end point
	 after us.  */
      EXECUTE_IF_SET_IN_BITMAP (hbb->m_liveout, 0, bit, bi)
	note_lr_end (ind2reg[bit], after_end_number);

      for (insn = hbb->m_last_insn; insn; insn = insn->m_prev)
	{
	  unsigned opi;
	  unsigned ndefs = insn->input_count ();
	  for (opi = 0; opi < insn->operand_count (); opi++)
	    {
	      gcc_checking_assert (insn->get_op (opi));
	      hsa_op_reg **regaddr = insn_reg_addr (insn, opi);
	      if (regaddr)
		{
		  hsa_op_reg *reg = *regaddr;
		  if (opi < ndefs)
		    note_lr_begin (reg, insn->m_number);
		  else
		    note_lr_end (reg, insn->m_number);
		}
	    }
	}

      /* Everything live-in in this BB has a start point before
	 our first insn.  */
      int before_start_number;
      if (hbb->m_first_insn)
	before_start_number = hbb->m_first_insn->m_number;
      else
	before_start_number = after_end_number;
      before_start_number--;
      EXECUTE_IF_SET_IN_BITMAP (hbb->m_livein, 0, bit, bi)
	note_lr_begin (ind2reg[bit], before_start_number);

      if (hbb->m_first_insn)
	m_last_insn = hbb->m_first_insn;
    }

  for (i = 0; i < hsa_cfun->m_reg_count; i++)
    if (ind2reg[i])
      {
	/* All regs that have still their start at after all code actually
	   are defined at the start of the routine (prologue).  */
	if (ind2reg[i]->m_lr_begin == insn_order)
	  ind2reg[i]->m_lr_begin = 0;
	/* All regs that have no use but a def will have lr_end == 0,
	   they are actually live from def until after the insn they are
	   defined in.  */
	if (ind2reg[i]->m_lr_end == 0)
	  ind2reg[i]->m_lr_end = ind2reg[i]->m_lr_begin + 1;
      }

  /* Sort all intervals by increasing start point.  */
  gcc_assert (ind2reg.length () == (size_t) hsa_cfun->m_reg_count);

  if (flag_checking)
    for (unsigned i = 0; i < ind2reg.length (); i++)
      gcc_assert (ind2reg[i]);

  ind2reg.qsort (cmp_begin);
  for (i = 0; i < 4; i++)
    active[i].reserve_exact (hsa_cfun->m_reg_count);

  /* Now comes the linear scan allocation.  */
  for (i = 0; i < hsa_cfun->m_reg_count; i++)
    {
      hsa_op_reg *reg = ind2reg[i];
      if (!reg)
	continue;
      expire_old_intervals (reg, active, classes);
      int cl = m_reg_class_for_type (reg->m_type);
      if (try_alloc_reg (classes, reg) >= 0)
	{
	  unsigned place = active[cl].lower_bound (reg, cmp_end);
	  active[cl].quick_insert (place, reg);
	}
      else
	spill_at_interval (reg, active);

      /* Some interesting dumping as we go.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  reg%d: [%5d, %5d)->",
		   reg->m_order, reg->m_lr_begin, reg->m_lr_end);
	  if (reg->m_reg_class)
	    fprintf (dump_file, "$%c%i", reg->m_reg_class, reg->m_hard_num);
	  else
	    fprintf (dump_file, "[%%__%s_%i]",
		     hsa_seg_name (reg->m_spill_sym->m_segment),
		     reg->m_spill_sym->m_name_number);
	  for (int cl = 0; cl < 4; cl++)
	    {
	      bool first = true;
	      hsa_op_reg *r;
	      fprintf (dump_file, " {");
	      for (int j = 0; active[cl].iterate (j, &r); j++)
		if (first)
		  {
		    fprintf (dump_file, "%d", r->m_order);
		    first = false;
		  }
		else
		  fprintf (dump_file, ", %d", r->m_order);
	      fprintf (dump_file, "}");
	    }
	  fprintf (dump_file, "\n");
	}
    }

  BITMAP_FREE (work);
  free (bbs);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "------- After liveness: -------\n");
      dump_hsa_cfun_regalloc (dump_file);
      fprintf (dump_file, "  ----- Intervals:\n");
      for (i = 0; i < hsa_cfun->m_reg_count; i++)
	{
	  hsa_op_reg *reg = ind2reg[i];
	  if (!reg)
	    continue;
	  fprintf (dump_file, "  reg%d: [%5d, %5d)->", reg->m_order,
		   reg->m_lr_begin, reg->m_lr_end);
	  if (reg->m_reg_class)
	    fprintf (dump_file, "$%c%i\n", reg->m_reg_class, reg->m_hard_num);
	  else
	    fprintf (dump_file, "[%%__%s_%i]\n",
		     hsa_seg_name (reg->m_spill_sym->m_segment),
		     reg->m_spill_sym->m_name_number);
	}
    }

  for (i = 0; i < 4; i++)
    active[i].release ();
  ind2reg.release ();
}

/* Entry point for register allocation.  */

static void
regalloc (void)
{
  basic_block bb;
  m_reg_class_desc classes[4];

  /* If there are no registers used in the function, exit right away.  */
  if (hsa_cfun->m_reg_count == 0)
    return;

  memset (classes, 0, sizeof (classes));
  classes[0].next_avail = 0;
  classes[0].max_num = 7;
  classes[0].cl_char = 'c';
  classes[1].cl_char = 's';
  classes[2].cl_char = 'd';
  classes[3].cl_char = 'q';

  for (int i = 1; i < 4; i++)
    {
      classes[i].next_avail = 0;
      classes[i].max_num = 20;
    }

  linear_scan_regalloc (classes);

  FOR_ALL_BB_FN (bb, cfun)
    rewrite_code_bb (bb, classes);
}

/* Out of SSA and register allocation on HSAIL IL.  */

void
hsa_regalloc (void)
{
  hsa_cfun->update_dominance ();
  naive_outof_ssa ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "------- After out-of-SSA: -------\n");
      dump_hsa_cfun (dump_file);
    }

  regalloc ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "------- After register allocation: -------\n");
      dump_hsa_cfun (dump_file);
    }
}
