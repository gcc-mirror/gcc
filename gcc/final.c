/* Convert RTL to assembler code and output it, for GNU compiler.
   Copyright (C) 1987-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This is the final pass of the compiler.
   It looks at the rtl code for a function and outputs assembler code.

   Call `final_start_function' to output the assembler code for function entry,
   `final' to output assembler code for some RTL code,
   `final_end_function' to output assembler code for function exit.
   If a function is compiled in several pieces, each piece is
   output separately with `final'.

   Some optimizations are also done at this level.
   Move instructions that were made unnecessary by good register allocation
   are detected and omitted from the output.  (Though most of these
   are removed by the last jump pass.)

   Instructions to set the condition codes are omitted when it can be
   seen that the condition codes already had the desired values.

   In some cases it is sufficient if the inherited condition codes
   have related values, but this may require the following insn
   (the one that tests the condition codes) to be modified.

   The code for the function prologue and epilogue are generated
   directly in assembler by the target functions function_prologue and
   function_epilogue.  Those instructions never exist as rtl.  */

#include "config.h"
#define INCLUDE_ALGORITHM /* reverse */
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "insn-config.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cgraph.h"
#include "tree-pretty-print.h" /* for dump_function_header */
#include "varasm.h"
#include "insn-attr.h"
#include "conditions.h"
#include "flags.h"
#include "output.h"
#include "except.h"
#include "rtl-error.h"
#include "toplev.h" /* exact_log2, floor_log2 */
#include "reload.h"
#include "intl.h"
#include "cfgrtl.h"
#include "debug.h"
#include "tree-pass.h"
#include "tree-ssa.h"
#include "cfgloop.h"
#include "params.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "rtl-iter.h"
#include "print-rtl.h"

#ifdef XCOFF_DEBUGGING_INFO
#include "xcoffout.h"		/* Needed for external data declarations.  */
#endif

#include "dwarf2out.h"

#ifdef DBX_DEBUGGING_INFO
#include "dbxout.h"
#endif

#include "sdbout.h"

/* Most ports that aren't using cc0 don't need to define CC_STATUS_INIT.
   So define a null default for it to save conditionalization later.  */
#ifndef CC_STATUS_INIT
#define CC_STATUS_INIT
#endif

/* Is the given character a logical line separator for the assembler?  */
#ifndef IS_ASM_LOGICAL_LINE_SEPARATOR
#define IS_ASM_LOGICAL_LINE_SEPARATOR(C, STR) ((C) == ';')
#endif

#ifndef JUMP_TABLES_IN_TEXT_SECTION
#define JUMP_TABLES_IN_TEXT_SECTION 0
#endif

/* Bitflags used by final_scan_insn.  */
#define SEEN_NOTE	1
#define SEEN_EMITTED	2

/* Last insn processed by final_scan_insn.  */
static rtx_insn *debug_insn;
rtx_insn *current_output_insn;

/* Line number of last NOTE.  */
static int last_linenum;

/* Column number of last NOTE.  */
static int last_columnnum;

/* Last discriminator written to assembly.  */
static int last_discriminator;

/* Discriminator of current block.  */
static int discriminator;

/* Highest line number in current block.  */
static int high_block_linenum;

/* Likewise for function.  */
static int high_function_linenum;

/* Filename of last NOTE.  */
static const char *last_filename;

/* Override filename, line and column number.  */
static const char *override_filename;
static int override_linenum;
static int override_columnnum;

/* Whether to force emission of a line note before the next insn.  */
static bool force_source_line = false;

extern const int length_unit_log; /* This is defined in insn-attrtab.c.  */

/* Nonzero while outputting an `asm' with operands.
   This means that inconsistencies are the user's fault, so don't die.
   The precise value is the insn being output, to pass to error_for_asm.  */
const rtx_insn *this_is_asm_operands;

/* Number of operands of this insn, for an `asm' with operands.  */
static unsigned int insn_noperands;

/* Compare optimization flag.  */

static rtx last_ignored_compare = 0;

/* Assign a unique number to each insn that is output.
   This can be used to generate unique local labels.  */

static int insn_counter = 0;

/* This variable contains machine-dependent flags (defined in tm.h)
   set and examined by output routines
   that describe how to interpret the condition codes properly.  */

CC_STATUS cc_status;

/* During output of an insn, this contains a copy of cc_status
   from before the insn.  */

CC_STATUS cc_prev_status;

/* Number of unmatched NOTE_INSN_BLOCK_BEG notes we have seen.  */

static int block_depth;

/* Nonzero if have enabled APP processing of our assembler output.  */

static int app_on;

/* If we are outputting an insn sequence, this contains the sequence rtx.
   Zero otherwise.  */

rtx_sequence *final_sequence;

#ifdef ASSEMBLER_DIALECT

/* Number of the assembler dialect to use, starting at 0.  */
static int dialect_number;
#endif

/* Nonnull if the insn currently being emitted was a COND_EXEC pattern.  */
rtx current_insn_predicate;

/* True if printing into -fdump-final-insns= dump.  */   
bool final_insns_dump_p;

/* True if profile_function should be called, but hasn't been called yet.  */
static bool need_profile_function;

static int asm_insn_count (rtx);
static void profile_function (FILE *);
static void profile_after_prologue (FILE *);
static bool notice_source_line (rtx_insn *, bool *);
static rtx walk_alter_subreg (rtx *, bool *);
static void output_asm_name (void);
static void output_alternate_entry_point (FILE *, rtx_insn *);
static tree get_mem_expr_from_op (rtx, int *);
static void output_asm_operand_names (rtx *, int *, int);
#ifdef LEAF_REGISTERS
static void leaf_renumber_regs (rtx_insn *);
#endif
#if HAVE_cc0
static int alter_cond (rtx);
#endif
static int align_fuzz (rtx, rtx, int, unsigned);
static void collect_fn_hard_reg_usage (void);
static tree get_call_fndecl (rtx_insn *);

/* Initialize data in final at the beginning of a compilation.  */

void
init_final (const char *filename ATTRIBUTE_UNUSED)
{
  app_on = 0;
  final_sequence = 0;

#ifdef ASSEMBLER_DIALECT
  dialect_number = ASSEMBLER_DIALECT;
#endif
}

/* Default target function prologue and epilogue assembler output.

   If not overridden for epilogue code, then the function body itself
   contains return instructions wherever needed.  */
void
default_function_pro_epilogue (FILE *)
{
}

void
default_function_switched_text_sections (FILE *file ATTRIBUTE_UNUSED,
					 tree decl ATTRIBUTE_UNUSED,
					 bool new_is_cold ATTRIBUTE_UNUSED)
{
}

/* Default target hook that outputs nothing to a stream.  */
void
no_asm_to_stream (FILE *file ATTRIBUTE_UNUSED)
{
}

/* Enable APP processing of subsequent output.
   Used before the output from an `asm' statement.  */

void
app_enable (void)
{
  if (! app_on)
    {
      fputs (ASM_APP_ON, asm_out_file);
      app_on = 1;
    }
}

/* Disable APP processing of subsequent output.
   Called from varasm.c before most kinds of output.  */

void
app_disable (void)
{
  if (app_on)
    {
      fputs (ASM_APP_OFF, asm_out_file);
      app_on = 0;
    }
}

/* Return the number of slots filled in the current
   delayed branch sequence (we don't count the insn needing the
   delay slot).   Zero if not in a delayed branch sequence.  */

int
dbr_sequence_length (void)
{
  if (final_sequence != 0)
    return XVECLEN (final_sequence, 0) - 1;
  else
    return 0;
}

/* The next two pages contain routines used to compute the length of an insn
   and to shorten branches.  */

/* Arrays for insn lengths, and addresses.  The latter is referenced by
   `insn_current_length'.  */

static int *insn_lengths;

vec<int> insn_addresses_;

/* Max uid for which the above arrays are valid.  */
static int insn_lengths_max_uid;

/* Address of insn being processed.  Used by `insn_current_length'.  */
int insn_current_address;

/* Address of insn being processed in previous iteration.  */
int insn_last_address;

/* known invariant alignment of insn being processed.  */
int insn_current_align;

/* After shorten_branches, for any insn, uid_align[INSN_UID (insn)]
   gives the next following alignment insn that increases the known
   alignment, or NULL_RTX if there is no such insn.
   For any alignment obtained this way, we can again index uid_align with
   its uid to obtain the next following align that in turn increases the
   alignment, till we reach NULL_RTX; the sequence obtained this way
   for each insn we'll call the alignment chain of this insn in the following
   comments.  */

struct label_alignment
{
  short alignment;
  short max_skip;
};

static rtx *uid_align;
static int *uid_shuid;
static struct label_alignment *label_align;

/* Indicate that branch shortening hasn't yet been done.  */

void
init_insn_lengths (void)
{
  if (uid_shuid)
    {
      free (uid_shuid);
      uid_shuid = 0;
    }
  if (insn_lengths)
    {
      free (insn_lengths);
      insn_lengths = 0;
      insn_lengths_max_uid = 0;
    }
  if (HAVE_ATTR_length)
    INSN_ADDRESSES_FREE ();
  if (uid_align)
    {
      free (uid_align);
      uid_align = 0;
    }
}

/* Obtain the current length of an insn.  If branch shortening has been done,
   get its actual length.  Otherwise, use FALLBACK_FN to calculate the
   length.  */
static int
get_attr_length_1 (rtx_insn *insn, int (*fallback_fn) (rtx_insn *))
{
  rtx body;
  int i;
  int length = 0;

  if (!HAVE_ATTR_length)
    return 0;

  if (insn_lengths_max_uid > INSN_UID (insn))
    return insn_lengths[INSN_UID (insn)];
  else
    switch (GET_CODE (insn))
      {
      case NOTE:
      case BARRIER:
      case CODE_LABEL:
      case DEBUG_INSN:
	return 0;

      case CALL_INSN:
      case JUMP_INSN:
	length = fallback_fn (insn);
	break;

      case INSN:
	body = PATTERN (insn);
	if (GET_CODE (body) == USE || GET_CODE (body) == CLOBBER)
	  return 0;

	else if (GET_CODE (body) == ASM_INPUT || asm_noperands (body) >= 0)
	  length = asm_insn_count (body) * fallback_fn (insn);
	else if (rtx_sequence *seq = dyn_cast <rtx_sequence *> (body))
	  for (i = 0; i < seq->len (); i++)
	    length += get_attr_length_1 (seq->insn (i), fallback_fn);
	else
	  length = fallback_fn (insn);
	break;

      default:
	break;
      }

#ifdef ADJUST_INSN_LENGTH
  ADJUST_INSN_LENGTH (insn, length);
#endif
  return length;
}

/* Obtain the current length of an insn.  If branch shortening has been done,
   get its actual length.  Otherwise, get its maximum length.  */
int
get_attr_length (rtx_insn *insn)
{
  return get_attr_length_1 (insn, insn_default_length);
}

/* Obtain the current length of an insn.  If branch shortening has been done,
   get its actual length.  Otherwise, get its minimum length.  */
int
get_attr_min_length (rtx_insn *insn)
{
  return get_attr_length_1 (insn, insn_min_length);
}

/* Code to handle alignment inside shorten_branches.  */

/* Here is an explanation how the algorithm in align_fuzz can give
   proper results:

   Call a sequence of instructions beginning with alignment point X
   and continuing until the next alignment point `block X'.  When `X'
   is used in an expression, it means the alignment value of the
   alignment point.

   Call the distance between the start of the first insn of block X, and
   the end of the last insn of block X `IX', for the `inner size of X'.
   This is clearly the sum of the instruction lengths.

   Likewise with the next alignment-delimited block following X, which we
   shall call block Y.

   Call the distance between the start of the first insn of block X, and
   the start of the first insn of block Y `OX', for the `outer size of X'.

   The estimated padding is then OX - IX.

   OX can be safely estimated as

           if (X >= Y)
                   OX = round_up(IX, Y)
           else
                   OX = round_up(IX, X) + Y - X

   Clearly est(IX) >= real(IX), because that only depends on the
   instruction lengths, and those being overestimated is a given.

   Clearly round_up(foo, Z) >= round_up(bar, Z) if foo >= bar, so
   we needn't worry about that when thinking about OX.

   When X >= Y, the alignment provided by Y adds no uncertainty factor
   for branch ranges starting before X, so we can just round what we have.
   But when X < Y, we don't know anything about the, so to speak,
   `middle bits', so we have to assume the worst when aligning up from an
   address mod X to one mod Y, which is Y - X.  */

#ifndef LABEL_ALIGN
#define LABEL_ALIGN(LABEL) align_labels_log
#endif

#ifndef LOOP_ALIGN
#define LOOP_ALIGN(LABEL) align_loops_log
#endif

#ifndef LABEL_ALIGN_AFTER_BARRIER
#define LABEL_ALIGN_AFTER_BARRIER(LABEL) 0
#endif

#ifndef JUMP_ALIGN
#define JUMP_ALIGN(LABEL) align_jumps_log
#endif

int
default_label_align_after_barrier_max_skip (rtx_insn *insn ATTRIBUTE_UNUSED)
{
  return 0;
}

int
default_loop_align_max_skip (rtx_insn *insn ATTRIBUTE_UNUSED)
{
  return align_loops_max_skip;
}

int
default_label_align_max_skip (rtx_insn *insn ATTRIBUTE_UNUSED)
{
  return align_labels_max_skip;
}

int
default_jump_align_max_skip (rtx_insn *insn ATTRIBUTE_UNUSED)
{
  return align_jumps_max_skip;
}

#ifndef ADDR_VEC_ALIGN
static int
final_addr_vec_align (rtx_jump_table_data *addr_vec)
{
  int align = GET_MODE_SIZE (addr_vec->get_data_mode ());

  if (align > BIGGEST_ALIGNMENT / BITS_PER_UNIT)
    align = BIGGEST_ALIGNMENT / BITS_PER_UNIT;
  return exact_log2 (align);

}

#define ADDR_VEC_ALIGN(ADDR_VEC) final_addr_vec_align (ADDR_VEC)
#endif

#ifndef INSN_LENGTH_ALIGNMENT
#define INSN_LENGTH_ALIGNMENT(INSN) length_unit_log
#endif

#define INSN_SHUID(INSN) (uid_shuid[INSN_UID (INSN)])

static int min_labelno, max_labelno;

#define LABEL_TO_ALIGNMENT(LABEL) \
  (label_align[CODE_LABEL_NUMBER (LABEL) - min_labelno].alignment)

#define LABEL_TO_MAX_SKIP(LABEL) \
  (label_align[CODE_LABEL_NUMBER (LABEL) - min_labelno].max_skip)

/* For the benefit of port specific code do this also as a function.  */

int
label_to_alignment (rtx label)
{
  if (CODE_LABEL_NUMBER (label) <= max_labelno)
    return LABEL_TO_ALIGNMENT (label);
  return 0;
}

int
label_to_max_skip (rtx label)
{
  if (CODE_LABEL_NUMBER (label) <= max_labelno)
    return LABEL_TO_MAX_SKIP (label);
  return 0;
}

/* The differences in addresses
   between a branch and its target might grow or shrink depending on
   the alignment the start insn of the range (the branch for a forward
   branch or the label for a backward branch) starts out on; if these
   differences are used naively, they can even oscillate infinitely.
   We therefore want to compute a 'worst case' address difference that
   is independent of the alignment the start insn of the range end
   up on, and that is at least as large as the actual difference.
   The function align_fuzz calculates the amount we have to add to the
   naively computed difference, by traversing the part of the alignment
   chain of the start insn of the range that is in front of the end insn
   of the range, and considering for each alignment the maximum amount
   that it might contribute to a size increase.

   For casesi tables, we also want to know worst case minimum amounts of
   address difference, in case a machine description wants to introduce
   some common offset that is added to all offsets in a table.
   For this purpose, align_fuzz with a growth argument of 0 computes the
   appropriate adjustment.  */

/* Compute the maximum delta by which the difference of the addresses of
   START and END might grow / shrink due to a different address for start
   which changes the size of alignment insns between START and END.
   KNOWN_ALIGN_LOG is the alignment known for START.
   GROWTH should be ~0 if the objective is to compute potential code size
   increase, and 0 if the objective is to compute potential shrink.
   The return value is undefined for any other value of GROWTH.  */

static int
align_fuzz (rtx start, rtx end, int known_align_log, unsigned int growth)
{
  int uid = INSN_UID (start);
  rtx align_label;
  int known_align = 1 << known_align_log;
  int end_shuid = INSN_SHUID (end);
  int fuzz = 0;

  for (align_label = uid_align[uid]; align_label; align_label = uid_align[uid])
    {
      int align_addr, new_align;

      uid = INSN_UID (align_label);
      align_addr = INSN_ADDRESSES (uid) - insn_lengths[uid];
      if (uid_shuid[uid] > end_shuid)
	break;
      known_align_log = LABEL_TO_ALIGNMENT (align_label);
      new_align = 1 << known_align_log;
      if (new_align < known_align)
	continue;
      fuzz += (-align_addr ^ growth) & (new_align - known_align);
      known_align = new_align;
    }
  return fuzz;
}

/* Compute a worst-case reference address of a branch so that it
   can be safely used in the presence of aligned labels.  Since the
   size of the branch itself is unknown, the size of the branch is
   not included in the range.  I.e. for a forward branch, the reference
   address is the end address of the branch as known from the previous
   branch shortening pass, minus a value to account for possible size
   increase due to alignment.  For a backward branch, it is the start
   address of the branch as known from the current pass, plus a value
   to account for possible size increase due to alignment.
   NB.: Therefore, the maximum offset allowed for backward branches needs
   to exclude the branch size.  */

int
insn_current_reference_address (rtx_insn *branch)
{
  rtx dest;
  int seq_uid;

  if (! INSN_ADDRESSES_SET_P ())
    return 0;

  rtx_insn *seq = NEXT_INSN (PREV_INSN (branch));
  seq_uid = INSN_UID (seq);
  if (!JUMP_P (branch))
    /* This can happen for example on the PA; the objective is to know the
       offset to address something in front of the start of the function.
       Thus, we can treat it like a backward branch.
       We assume here that FUNCTION_BOUNDARY / BITS_PER_UNIT is larger than
       any alignment we'd encounter, so we skip the call to align_fuzz.  */
    return insn_current_address;
  dest = JUMP_LABEL (branch);

  /* BRANCH has no proper alignment chain set, so use SEQ.
     BRANCH also has no INSN_SHUID.  */
  if (INSN_SHUID (seq) < INSN_SHUID (dest))
    {
      /* Forward branch.  */
      return (insn_last_address + insn_lengths[seq_uid]
	      - align_fuzz (seq, dest, length_unit_log, ~0));
    }
  else
    {
      /* Backward branch.  */
      return (insn_current_address
	      + align_fuzz (dest, seq, length_unit_log, ~0));
    }
}

/* Compute branch alignments based on frequency information in the
   CFG.  */

unsigned int
compute_alignments (void)
{
  int log, max_skip, max_log;
  basic_block bb;
  int freq_max = 0;
  int freq_threshold = 0;

  if (label_align)
    {
      free (label_align);
      label_align = 0;
    }

  max_labelno = max_label_num ();
  min_labelno = get_first_label_num ();
  label_align = XCNEWVEC (struct label_alignment, max_labelno - min_labelno + 1);

  /* If not optimizing or optimizing for size, don't assign any alignments.  */
  if (! optimize || optimize_function_for_size_p (cfun))
    return 0;

  if (dump_file)
    {
      dump_reg_info (dump_file);
      dump_flow_info (dump_file, TDF_DETAILS);
      flow_loops_dump (dump_file, NULL, 1);
    }
  loop_optimizer_init (AVOID_CFG_MODIFICATIONS);
  FOR_EACH_BB_FN (bb, cfun)
    if (bb->frequency > freq_max)
      freq_max = bb->frequency;
  freq_threshold = freq_max / PARAM_VALUE (PARAM_ALIGN_THRESHOLD);

  if (dump_file)
    fprintf (dump_file, "freq_max: %i\n",freq_max);
  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *label = BB_HEAD (bb);
      int fallthru_frequency = 0, branch_frequency = 0, has_fallthru = 0;
      edge e;
      edge_iterator ei;

      if (!LABEL_P (label)
	  || optimize_bb_for_size_p (bb))
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "BB %4i freq %4i loop %2i loop_depth %2i skipped.\n",
		     bb->index, bb->frequency, bb->loop_father->num,
		     bb_loop_depth (bb));
	  continue;
	}
      max_log = LABEL_ALIGN (label);
      max_skip = targetm.asm_out.label_align_max_skip (label);

      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (e->flags & EDGE_FALLTHRU)
	    has_fallthru = 1, fallthru_frequency += EDGE_FREQUENCY (e);
	  else
	    branch_frequency += EDGE_FREQUENCY (e);
	}
      if (dump_file)
	{
	  fprintf (dump_file, "BB %4i freq %4i loop %2i loop_depth"
		   " %2i fall %4i branch %4i",
		   bb->index, bb->frequency, bb->loop_father->num,
		   bb_loop_depth (bb),
		   fallthru_frequency, branch_frequency);
	  if (!bb->loop_father->inner && bb->loop_father->num)
	    fprintf (dump_file, " inner_loop");
	  if (bb->loop_father->header == bb)
	    fprintf (dump_file, " loop_header");
	  fprintf (dump_file, "\n");
	}

      /* There are two purposes to align block with no fallthru incoming edge:
	 1) to avoid fetch stalls when branch destination is near cache boundary
	 2) to improve cache efficiency in case the previous block is not executed
	    (so it does not need to be in the cache).

	 We to catch first case, we align frequently executed blocks.
	 To catch the second, we align blocks that are executed more frequently
	 than the predecessor and the predecessor is likely to not be executed
	 when function is called.  */

      if (!has_fallthru
	  && (branch_frequency > freq_threshold
	      || (bb->frequency > bb->prev_bb->frequency * 10
		  && (bb->prev_bb->frequency
		      <= ENTRY_BLOCK_PTR_FOR_FN (cfun)->frequency / 2))))
	{
	  log = JUMP_ALIGN (label);
	  if (dump_file)
	    fprintf (dump_file, "  jump alignment added.\n");
	  if (max_log < log)
	    {
	      max_log = log;
	      max_skip = targetm.asm_out.jump_align_max_skip (label);
	    }
	}
      /* In case block is frequent and reached mostly by non-fallthru edge,
	 align it.  It is most likely a first block of loop.  */
      if (has_fallthru
	  && !(single_succ_p (bb)
	       && single_succ (bb) == EXIT_BLOCK_PTR_FOR_FN (cfun))
	  && optimize_bb_for_speed_p (bb)
	  && branch_frequency + fallthru_frequency > freq_threshold
	  && (branch_frequency
	      > fallthru_frequency * PARAM_VALUE (PARAM_ALIGN_LOOP_ITERATIONS)))
	{
	  log = LOOP_ALIGN (label);
	  if (dump_file)
	    fprintf (dump_file, "  internal loop alignment added.\n");
	  if (max_log < log)
	    {
	      max_log = log;
	      max_skip = targetm.asm_out.loop_align_max_skip (label);
	    }
	}
      LABEL_TO_ALIGNMENT (label) = max_log;
      LABEL_TO_MAX_SKIP (label) = max_skip;
    }

  loop_optimizer_finalize ();
  free_dominance_info (CDI_DOMINATORS);
  return 0;
}

/* Grow the LABEL_ALIGN array after new labels are created.  */

static void 
grow_label_align (void)
{
  int old = max_labelno;
  int n_labels;
  int n_old_labels;

  max_labelno = max_label_num ();

  n_labels = max_labelno - min_labelno + 1;
  n_old_labels = old - min_labelno + 1;

  label_align = XRESIZEVEC (struct label_alignment, label_align, n_labels);

  /* Range of labels grows monotonically in the function.  Failing here
     means that the initialization of array got lost.  */
  gcc_assert (n_old_labels <= n_labels);

  memset (label_align + n_old_labels, 0,
          (n_labels - n_old_labels) * sizeof (struct label_alignment));
}

/* Update the already computed alignment information.  LABEL_PAIRS is a vector
   made up of pairs of labels for which the alignment information of the first
   element will be copied from that of the second element.  */

void
update_alignments (vec<rtx> &label_pairs)
{
  unsigned int i = 0;
  rtx iter, label = NULL_RTX;

  if (max_labelno != max_label_num ())
    grow_label_align ();

  FOR_EACH_VEC_ELT (label_pairs, i, iter)
    if (i & 1)
      {
	LABEL_TO_ALIGNMENT (label) = LABEL_TO_ALIGNMENT (iter);
	LABEL_TO_MAX_SKIP (label) = LABEL_TO_MAX_SKIP (iter);
      }
    else
      label = iter;
}

namespace {

const pass_data pass_data_compute_alignments =
{
  RTL_PASS, /* type */
  "alignments", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_compute_alignments : public rtl_opt_pass
{
public:
  pass_compute_alignments (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_compute_alignments, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *) { return compute_alignments (); }

}; // class pass_compute_alignments

} // anon namespace

rtl_opt_pass *
make_pass_compute_alignments (gcc::context *ctxt)
{
  return new pass_compute_alignments (ctxt);
}


/* Make a pass over all insns and compute their actual lengths by shortening
   any branches of variable length if possible.  */

/* shorten_branches might be called multiple times:  for example, the SH
   port splits out-of-range conditional branches in MACHINE_DEPENDENT_REORG.
   In order to do this, it needs proper length information, which it obtains
   by calling shorten_branches.  This cannot be collapsed with
   shorten_branches itself into a single pass unless we also want to integrate
   reorg.c, since the branch splitting exposes new instructions with delay
   slots.  */

void
shorten_branches (rtx_insn *first)
{
  rtx_insn *insn;
  int max_uid;
  int i;
  int max_log;
  int max_skip;
#define MAX_CODE_ALIGN 16
  rtx_insn *seq;
  int something_changed = 1;
  char *varying_length;
  rtx body;
  int uid;
  rtx align_tab[MAX_CODE_ALIGN];

  /* Compute maximum UID and allocate label_align / uid_shuid.  */
  max_uid = get_max_uid ();

  /* Free uid_shuid before reallocating it.  */
  free (uid_shuid);

  uid_shuid = XNEWVEC (int, max_uid);

  if (max_labelno != max_label_num ())
    grow_label_align ();

  /* Initialize label_align and set up uid_shuid to be strictly
     monotonically rising with insn order.  */
  /* We use max_log here to keep track of the maximum alignment we want to
     impose on the next CODE_LABEL (or the current one if we are processing
     the CODE_LABEL itself).  */

  max_log = 0;
  max_skip = 0;

  for (insn = get_insns (), i = 1; insn; insn = NEXT_INSN (insn))
    {
      int log;

      INSN_SHUID (insn) = i++;
      if (INSN_P (insn))
	continue;

      if (rtx_code_label *label = dyn_cast <rtx_code_label *> (insn))
	{
	  /* Merge in alignments computed by compute_alignments.  */
	  log = LABEL_TO_ALIGNMENT (label);
	  if (max_log < log)
	    {
	      max_log = log;
	      max_skip = LABEL_TO_MAX_SKIP (label);
	    }

	  rtx_jump_table_data *table = jump_table_for_label (label);
	  if (!table)
	    {
	      log = LABEL_ALIGN (label);
	      if (max_log < log)
		{
		  max_log = log;
		  max_skip = targetm.asm_out.label_align_max_skip (label);
		}
	    }
	  /* ADDR_VECs only take room if read-only data goes into the text
	     section.  */
	  if ((JUMP_TABLES_IN_TEXT_SECTION
	       || readonly_data_section == text_section)
	      && table)
	    {
	      log = ADDR_VEC_ALIGN (table);
	      if (max_log < log)
		{
		  max_log = log;
		  max_skip = targetm.asm_out.label_align_max_skip (label);
		}
	    }
	  LABEL_TO_ALIGNMENT (label) = max_log;
	  LABEL_TO_MAX_SKIP (label) = max_skip;
	  max_log = 0;
	  max_skip = 0;
	}
      else if (BARRIER_P (insn))
	{
	  rtx_insn *label;

	  for (label = insn; label && ! INSN_P (label);
	       label = NEXT_INSN (label))
	    if (LABEL_P (label))
	      {
		log = LABEL_ALIGN_AFTER_BARRIER (insn);
		if (max_log < log)
		  {
		    max_log = log;
		    max_skip = targetm.asm_out.label_align_after_barrier_max_skip (label);
		  }
		break;
	      }
	}
    }
  if (!HAVE_ATTR_length)
    return;

  /* Allocate the rest of the arrays.  */
  insn_lengths = XNEWVEC (int, max_uid);
  insn_lengths_max_uid = max_uid;
  /* Syntax errors can lead to labels being outside of the main insn stream.
     Initialize insn_addresses, so that we get reproducible results.  */
  INSN_ADDRESSES_ALLOC (max_uid);

  varying_length = XCNEWVEC (char, max_uid);

  /* Initialize uid_align.  We scan instructions
     from end to start, and keep in align_tab[n] the last seen insn
     that does an alignment of at least n+1, i.e. the successor
     in the alignment chain for an insn that does / has a known
     alignment of n.  */
  uid_align = XCNEWVEC (rtx, max_uid);

  for (i = MAX_CODE_ALIGN; --i >= 0;)
    align_tab[i] = NULL_RTX;
  seq = get_last_insn ();
  for (; seq; seq = PREV_INSN (seq))
    {
      int uid = INSN_UID (seq);
      int log;
      log = (LABEL_P (seq) ? LABEL_TO_ALIGNMENT (seq) : 0);
      uid_align[uid] = align_tab[0];
      if (log)
	{
	  /* Found an alignment label.  */
	  uid_align[uid] = align_tab[log];
	  for (i = log - 1; i >= 0; i--)
	    align_tab[i] = seq;
	}
    }

  /* When optimizing, we start assuming minimum length, and keep increasing
     lengths as we find the need for this, till nothing changes.
     When not optimizing, we start assuming maximum lengths, and
     do a single pass to update the lengths.  */
  bool increasing = optimize != 0;

#ifdef CASE_VECTOR_SHORTEN_MODE
  if (optimize)
    {
      /* Look for ADDR_DIFF_VECs, and initialize their minimum and maximum
         label fields.  */

      int min_shuid = INSN_SHUID (get_insns ()) - 1;
      int max_shuid = INSN_SHUID (get_last_insn ()) + 1;
      int rel;

      for (insn = first; insn != 0; insn = NEXT_INSN (insn))
	{
	  rtx min_lab = NULL_RTX, max_lab = NULL_RTX, pat;
	  int len, i, min, max, insn_shuid;
	  int min_align;
	  addr_diff_vec_flags flags;

	  if (! JUMP_TABLE_DATA_P (insn)
	      || GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC)
	    continue;
	  pat = PATTERN (insn);
	  len = XVECLEN (pat, 1);
	  gcc_assert (len > 0);
	  min_align = MAX_CODE_ALIGN;
	  for (min = max_shuid, max = min_shuid, i = len - 1; i >= 0; i--)
	    {
	      rtx lab = XEXP (XVECEXP (pat, 1, i), 0);
	      int shuid = INSN_SHUID (lab);
	      if (shuid < min)
		{
		  min = shuid;
		  min_lab = lab;
		}
	      if (shuid > max)
		{
		  max = shuid;
		  max_lab = lab;
		}
	      if (min_align > LABEL_TO_ALIGNMENT (lab))
		min_align = LABEL_TO_ALIGNMENT (lab);
	    }
	  XEXP (pat, 2) = gen_rtx_LABEL_REF (Pmode, min_lab);
	  XEXP (pat, 3) = gen_rtx_LABEL_REF (Pmode, max_lab);
	  insn_shuid = INSN_SHUID (insn);
	  rel = INSN_SHUID (XEXP (XEXP (pat, 0), 0));
	  memset (&flags, 0, sizeof (flags));
	  flags.min_align = min_align;
	  flags.base_after_vec = rel > insn_shuid;
	  flags.min_after_vec  = min > insn_shuid;
	  flags.max_after_vec  = max > insn_shuid;
	  flags.min_after_base = min > rel;
	  flags.max_after_base = max > rel;
	  ADDR_DIFF_VEC_FLAGS (pat) = flags;

	  if (increasing)
	    PUT_MODE (pat, CASE_VECTOR_SHORTEN_MODE (0, 0, pat));
	}
    }
#endif /* CASE_VECTOR_SHORTEN_MODE */

  /* Compute initial lengths, addresses, and varying flags for each insn.  */
  int (*length_fun) (rtx_insn *) = increasing ? insn_min_length : insn_default_length;

  for (insn_current_address = 0, insn = first;
       insn != 0;
       insn_current_address += insn_lengths[uid], insn = NEXT_INSN (insn))
    {
      uid = INSN_UID (insn);

      insn_lengths[uid] = 0;

      if (LABEL_P (insn))
	{
	  int log = LABEL_TO_ALIGNMENT (insn);
	  if (log)
	    {
	      int align = 1 << log;
	      int new_address = (insn_current_address + align - 1) & -align;
	      insn_lengths[uid] = new_address - insn_current_address;
	    }
	}

      INSN_ADDRESSES (uid) = insn_current_address + insn_lengths[uid];

      if (NOTE_P (insn) || BARRIER_P (insn)
	  || LABEL_P (insn) || DEBUG_INSN_P (insn))
	continue;
      if (insn->deleted ())
	continue;

      body = PATTERN (insn);
      if (rtx_jump_table_data *table = dyn_cast <rtx_jump_table_data *> (insn))
	{
	  /* This only takes room if read-only data goes into the text
	     section.  */
	  if (JUMP_TABLES_IN_TEXT_SECTION
	      || readonly_data_section == text_section)
	    insn_lengths[uid] = (XVECLEN (body,
					  GET_CODE (body) == ADDR_DIFF_VEC)
				 * GET_MODE_SIZE (table->get_data_mode ()));
	  /* Alignment is handled by ADDR_VEC_ALIGN.  */
	}
      else if (GET_CODE (body) == ASM_INPUT || asm_noperands (body) >= 0)
	insn_lengths[uid] = asm_insn_count (body) * insn_default_length (insn);
      else if (rtx_sequence *body_seq = dyn_cast <rtx_sequence *> (body))
	{
	  int i;
	  int const_delay_slots;
	  if (DELAY_SLOTS)
	    const_delay_slots = const_num_delay_slots (body_seq->insn (0));
	  else
	    const_delay_slots = 0;

	  int (*inner_length_fun) (rtx_insn *)
	    = const_delay_slots ? length_fun : insn_default_length;
	  /* Inside a delay slot sequence, we do not do any branch shortening
	     if the shortening could change the number of delay slots
	     of the branch.  */
	  for (i = 0; i < body_seq->len (); i++)
	    {
	      rtx_insn *inner_insn = body_seq->insn (i);
	      int inner_uid = INSN_UID (inner_insn);
	      int inner_length;

	      if (GET_CODE (PATTERN (inner_insn)) == ASM_INPUT
		  || asm_noperands (PATTERN (inner_insn)) >= 0)
		inner_length = (asm_insn_count (PATTERN (inner_insn))
				* insn_default_length (inner_insn));
	      else
		inner_length = inner_length_fun (inner_insn);

	      insn_lengths[inner_uid] = inner_length;
	      if (const_delay_slots)
		{
		  if ((varying_length[inner_uid]
		       = insn_variable_length_p (inner_insn)) != 0)
		    varying_length[uid] = 1;
		  INSN_ADDRESSES (inner_uid) = (insn_current_address
						+ insn_lengths[uid]);
		}
	      else
		varying_length[inner_uid] = 0;
	      insn_lengths[uid] += inner_length;
	    }
	}
      else if (GET_CODE (body) != USE && GET_CODE (body) != CLOBBER)
	{
	  insn_lengths[uid] = length_fun (insn);
	  varying_length[uid] = insn_variable_length_p (insn);
	}

      /* If needed, do any adjustment.  */
#ifdef ADJUST_INSN_LENGTH
      ADJUST_INSN_LENGTH (insn, insn_lengths[uid]);
      if (insn_lengths[uid] < 0)
	fatal_insn ("negative insn length", insn);
#endif
    }

  /* Now loop over all the insns finding varying length insns.  For each,
     get the current insn length.  If it has changed, reflect the change.
     When nothing changes for a full pass, we are done.  */

  while (something_changed)
    {
      something_changed = 0;
      insn_current_align = MAX_CODE_ALIGN - 1;
      for (insn_current_address = 0, insn = first;
	   insn != 0;
	   insn = NEXT_INSN (insn))
	{
	  int new_length;
#ifdef ADJUST_INSN_LENGTH
	  int tmp_length;
#endif
	  int length_align;

	  uid = INSN_UID (insn);

	  if (rtx_code_label *label = dyn_cast <rtx_code_label *> (insn))
	    {
	      int log = LABEL_TO_ALIGNMENT (label);

#ifdef CASE_VECTOR_SHORTEN_MODE
	      /* If the mode of a following jump table was changed, we
		 may need to update the alignment of this label.  */

	      if (JUMP_TABLES_IN_TEXT_SECTION
		  || readonly_data_section == text_section)
		{
		  rtx_jump_table_data *table = jump_table_for_label (label);
		  if (table)
		    {
		      int newlog = ADDR_VEC_ALIGN (table);
		      if (newlog != log)
			{
			  log = newlog;
			  LABEL_TO_ALIGNMENT (insn) = log;
			  something_changed = 1;
			}
		    }
		}
#endif

	      if (log > insn_current_align)
		{
		  int align = 1 << log;
		  int new_address= (insn_current_address + align - 1) & -align;
		  insn_lengths[uid] = new_address - insn_current_address;
		  insn_current_align = log;
		  insn_current_address = new_address;
		}
	      else
		insn_lengths[uid] = 0;
	      INSN_ADDRESSES (uid) = insn_current_address;
	      continue;
	    }

	  length_align = INSN_LENGTH_ALIGNMENT (insn);
	  if (length_align < insn_current_align)
	    insn_current_align = length_align;

	  insn_last_address = INSN_ADDRESSES (uid);
	  INSN_ADDRESSES (uid) = insn_current_address;

#ifdef CASE_VECTOR_SHORTEN_MODE
	  if (optimize
	      && JUMP_TABLE_DATA_P (insn)
	      && GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC)
	    {
	      rtx_jump_table_data *table = as_a <rtx_jump_table_data *> (insn);
	      rtx body = PATTERN (insn);
	      int old_length = insn_lengths[uid];
	      rtx_insn *rel_lab =
		safe_as_a <rtx_insn *> (XEXP (XEXP (body, 0), 0));
	      rtx min_lab = XEXP (XEXP (body, 2), 0);
	      rtx max_lab = XEXP (XEXP (body, 3), 0);
	      int rel_addr = INSN_ADDRESSES (INSN_UID (rel_lab));
	      int min_addr = INSN_ADDRESSES (INSN_UID (min_lab));
	      int max_addr = INSN_ADDRESSES (INSN_UID (max_lab));
	      rtx_insn *prev;
	      int rel_align = 0;
	      addr_diff_vec_flags flags;
	      machine_mode vec_mode;

	      /* Avoid automatic aggregate initialization.  */
	      flags = ADDR_DIFF_VEC_FLAGS (body);

	      /* Try to find a known alignment for rel_lab.  */
	      for (prev = rel_lab;
		   prev
		   && ! insn_lengths[INSN_UID (prev)]
		   && ! (varying_length[INSN_UID (prev)] & 1);
		   prev = PREV_INSN (prev))
		if (varying_length[INSN_UID (prev)] & 2)
		  {
		    rel_align = LABEL_TO_ALIGNMENT (prev);
		    break;
		  }

	      /* See the comment on addr_diff_vec_flags in rtl.h for the
		 meaning of the flags values.  base: REL_LAB   vec: INSN  */
	      /* Anything after INSN has still addresses from the last
		 pass; adjust these so that they reflect our current
		 estimate for this pass.  */
	      if (flags.base_after_vec)
		rel_addr += insn_current_address - insn_last_address;
	      if (flags.min_after_vec)
		min_addr += insn_current_address - insn_last_address;
	      if (flags.max_after_vec)
		max_addr += insn_current_address - insn_last_address;
	      /* We want to know the worst case, i.e. lowest possible value
		 for the offset of MIN_LAB.  If MIN_LAB is after REL_LAB,
		 its offset is positive, and we have to be wary of code shrink;
		 otherwise, it is negative, and we have to be vary of code
		 size increase.  */
	      if (flags.min_after_base)
		{
		  /* If INSN is between REL_LAB and MIN_LAB, the size
		     changes we are about to make can change the alignment
		     within the observed offset, therefore we have to break
		     it up into two parts that are independent.  */
		  if (! flags.base_after_vec && flags.min_after_vec)
		    {
		      min_addr -= align_fuzz (rel_lab, insn, rel_align, 0);
		      min_addr -= align_fuzz (insn, min_lab, 0, 0);
		    }
		  else
		    min_addr -= align_fuzz (rel_lab, min_lab, rel_align, 0);
		}
	      else
		{
		  if (flags.base_after_vec && ! flags.min_after_vec)
		    {
		      min_addr -= align_fuzz (min_lab, insn, 0, ~0);
		      min_addr -= align_fuzz (insn, rel_lab, 0, ~0);
		    }
		  else
		    min_addr -= align_fuzz (min_lab, rel_lab, 0, ~0);
		}
	      /* Likewise, determine the highest lowest possible value
		 for the offset of MAX_LAB.  */
	      if (flags.max_after_base)
		{
		  if (! flags.base_after_vec && flags.max_after_vec)
		    {
		      max_addr += align_fuzz (rel_lab, insn, rel_align, ~0);
		      max_addr += align_fuzz (insn, max_lab, 0, ~0);
		    }
		  else
		    max_addr += align_fuzz (rel_lab, max_lab, rel_align, ~0);
		}
	      else
		{
		  if (flags.base_after_vec && ! flags.max_after_vec)
		    {
		      max_addr += align_fuzz (max_lab, insn, 0, 0);
		      max_addr += align_fuzz (insn, rel_lab, 0, 0);
		    }
		  else
		    max_addr += align_fuzz (max_lab, rel_lab, 0, 0);
		}
	      vec_mode = CASE_VECTOR_SHORTEN_MODE (min_addr - rel_addr,
						   max_addr - rel_addr, body);
	      if (!increasing
		  || (GET_MODE_SIZE (vec_mode)
		      >= GET_MODE_SIZE (table->get_data_mode ())))
		PUT_MODE (body, vec_mode);
	      if (JUMP_TABLES_IN_TEXT_SECTION
		  || readonly_data_section == text_section)
		{
		  insn_lengths[uid]
		    = (XVECLEN (body, 1)
		       * GET_MODE_SIZE (table->get_data_mode ()));
		  insn_current_address += insn_lengths[uid];
		  if (insn_lengths[uid] != old_length)
		    something_changed = 1;
		}

	      continue;
	    }
#endif /* CASE_VECTOR_SHORTEN_MODE */

	  if (! (varying_length[uid]))
	    {
	      if (NONJUMP_INSN_P (insn)
		  && GET_CODE (PATTERN (insn)) == SEQUENCE)
		{
		  int i;

		  body = PATTERN (insn);
		  for (i = 0; i < XVECLEN (body, 0); i++)
		    {
		      rtx inner_insn = XVECEXP (body, 0, i);
		      int inner_uid = INSN_UID (inner_insn);

		      INSN_ADDRESSES (inner_uid) = insn_current_address;

		      insn_current_address += insn_lengths[inner_uid];
		    }
		}
	      else
		insn_current_address += insn_lengths[uid];

	      continue;
	    }

	  if (NONJUMP_INSN_P (insn) && GET_CODE (PATTERN (insn)) == SEQUENCE)
	    {
	      rtx_sequence *seqn = as_a <rtx_sequence *> (PATTERN (insn));
	      int i;

	      body = PATTERN (insn);
	      new_length = 0;
	      for (i = 0; i < seqn->len (); i++)
		{
		  rtx_insn *inner_insn = seqn->insn (i);
		  int inner_uid = INSN_UID (inner_insn);
		  int inner_length;

		  INSN_ADDRESSES (inner_uid) = insn_current_address;

		  /* insn_current_length returns 0 for insns with a
		     non-varying length.  */
		  if (! varying_length[inner_uid])
		    inner_length = insn_lengths[inner_uid];
		  else
		    inner_length = insn_current_length (inner_insn);

		  if (inner_length != insn_lengths[inner_uid])
		    {
		      if (!increasing || inner_length > insn_lengths[inner_uid])
			{
			  insn_lengths[inner_uid] = inner_length;
			  something_changed = 1;
			}
		      else
			inner_length = insn_lengths[inner_uid];
		    }
		  insn_current_address += inner_length;
		  new_length += inner_length;
		}
	    }
	  else
	    {
	      new_length = insn_current_length (insn);
	      insn_current_address += new_length;
	    }

#ifdef ADJUST_INSN_LENGTH
	  /* If needed, do any adjustment.  */
	  tmp_length = new_length;
	  ADJUST_INSN_LENGTH (insn, new_length);
	  insn_current_address += (new_length - tmp_length);
#endif

	  if (new_length != insn_lengths[uid]
	      && (!increasing || new_length > insn_lengths[uid]))
	    {
	      insn_lengths[uid] = new_length;
	      something_changed = 1;
	    }
	  else
	    insn_current_address += insn_lengths[uid] - new_length;
	}
      /* For a non-optimizing compile, do only a single pass.  */
      if (!increasing)
	break;
    }
  crtl->max_insn_address = insn_current_address;
  free (varying_length);
}

/* Given the body of an INSN known to be generated by an ASM statement, return
   the number of machine instructions likely to be generated for this insn.
   This is used to compute its length.  */

static int
asm_insn_count (rtx body)
{
  const char *templ;

  if (GET_CODE (body) == ASM_INPUT)
    templ = XSTR (body, 0);
  else
    templ = decode_asm_operands (body, NULL, NULL, NULL, NULL, NULL);

  return asm_str_count (templ);
}

/* Return the number of machine instructions likely to be generated for the
   inline-asm template. */
int
asm_str_count (const char *templ)
{
  int count = 1;

  if (!*templ)
    return 0;

  for (; *templ; templ++)
    if (IS_ASM_LOGICAL_LINE_SEPARATOR (*templ, templ)
	|| *templ == '\n')
      count++;

  return count;
}

/* ??? This is probably the wrong place for these.  */
/* Structure recording the mapping from source file and directory
   names at compile time to those to be embedded in debug
   information.  */
struct debug_prefix_map
{
  const char *old_prefix;
  const char *new_prefix;
  size_t old_len;
  size_t new_len;
  struct debug_prefix_map *next;
};

/* Linked list of such structures.  */
static debug_prefix_map *debug_prefix_maps;


/* Record a debug file prefix mapping.  ARG is the argument to
   -fdebug-prefix-map and must be of the form OLD=NEW.  */

void
add_debug_prefix_map (const char *arg)
{
  debug_prefix_map *map;
  const char *p;

  p = strchr (arg, '=');
  if (!p)
    {
      error ("invalid argument %qs to -fdebug-prefix-map", arg);
      return;
    }
  map = XNEW (debug_prefix_map);
  map->old_prefix = xstrndup (arg, p - arg);
  map->old_len = p - arg;
  p++;
  map->new_prefix = xstrdup (p);
  map->new_len = strlen (p);
  map->next = debug_prefix_maps;
  debug_prefix_maps = map;
}

/* Perform user-specified mapping of debug filename prefixes.  Return
   the new name corresponding to FILENAME.  */

const char *
remap_debug_filename (const char *filename)
{
  debug_prefix_map *map;
  char *s;
  const char *name;
  size_t name_len;

  for (map = debug_prefix_maps; map; map = map->next)
    if (filename_ncmp (filename, map->old_prefix, map->old_len) == 0)
      break;
  if (!map)
    return filename;
  name = filename + map->old_len;
  name_len = strlen (name) + 1;
  s = (char *) alloca (name_len + map->new_len);
  memcpy (s, map->new_prefix, map->new_len);
  memcpy (s + map->new_len, name, name_len);
  return ggc_strdup (s);
}

/* Return true if DWARF2 debug info can be emitted for DECL.  */

static bool
dwarf2_debug_info_emitted_p (tree decl)
{
  if (write_symbols != DWARF2_DEBUG && write_symbols != VMS_AND_DWARF2_DEBUG)
    return false;

  if (DECL_IGNORED_P (decl))
    return false;

  return true;
}

/* Return scope resulting from combination of S1 and S2.  */
static tree
choose_inner_scope (tree s1, tree s2)
{
   if (!s1)
     return s2;
   if (!s2)
     return s1;
   if (BLOCK_NUMBER (s1) > BLOCK_NUMBER (s2))
     return s1;
   return s2;
}

/* Emit lexical block notes needed to change scope from S1 to S2.  */

static void
change_scope (rtx_insn *orig_insn, tree s1, tree s2)
{
  rtx_insn *insn = orig_insn;
  tree com = NULL_TREE;
  tree ts1 = s1, ts2 = s2;
  tree s;

  while (ts1 != ts2)
    {
      gcc_assert (ts1 && ts2);
      if (BLOCK_NUMBER (ts1) > BLOCK_NUMBER (ts2))
	ts1 = BLOCK_SUPERCONTEXT (ts1);
      else if (BLOCK_NUMBER (ts1) < BLOCK_NUMBER (ts2))
	ts2 = BLOCK_SUPERCONTEXT (ts2);
      else
	{
	  ts1 = BLOCK_SUPERCONTEXT (ts1);
	  ts2 = BLOCK_SUPERCONTEXT (ts2);
	}
    }
  com = ts1;

  /* Close scopes.  */
  s = s1;
  while (s != com)
    {
      rtx_note *note = emit_note_before (NOTE_INSN_BLOCK_END, insn);
      NOTE_BLOCK (note) = s;
      s = BLOCK_SUPERCONTEXT (s);
    }

  /* Open scopes.  */
  s = s2;
  while (s != com)
    {
      insn = emit_note_before (NOTE_INSN_BLOCK_BEG, insn);
      NOTE_BLOCK (insn) = s;
      s = BLOCK_SUPERCONTEXT (s);
    }
}

/* Rebuild all the NOTE_INSN_BLOCK_BEG and NOTE_INSN_BLOCK_END notes based
   on the scope tree and the newly reordered instructions.  */

static void
reemit_insn_block_notes (void)
{
  tree cur_block = DECL_INITIAL (cfun->decl);
  rtx_insn *insn;
  rtx_note *note;

  insn = get_insns ();
  for (; insn; insn = NEXT_INSN (insn))
    {
      tree this_block;

      /* Prevent lexical blocks from straddling section boundaries.  */
      if (NOTE_P (insn) && NOTE_KIND (insn) == NOTE_INSN_SWITCH_TEXT_SECTIONS)
        {
          for (tree s = cur_block; s != DECL_INITIAL (cfun->decl);
               s = BLOCK_SUPERCONTEXT (s))
            {
              rtx_note *note = emit_note_before (NOTE_INSN_BLOCK_END, insn);
              NOTE_BLOCK (note) = s;
              note = emit_note_after (NOTE_INSN_BLOCK_BEG, insn);
              NOTE_BLOCK (note) = s;
            }
        }

      if (!active_insn_p (insn))
        continue;

      /* Avoid putting scope notes between jump table and its label.  */
      if (JUMP_TABLE_DATA_P (insn))
	continue;

      this_block = insn_scope (insn);
      /* For sequences compute scope resulting from merging all scopes
	 of instructions nested inside.  */
      if (rtx_sequence *body = dyn_cast <rtx_sequence *> (PATTERN (insn)))
	{
	  int i;

	  this_block = NULL;
	  for (i = 0; i < body->len (); i++)
	    this_block = choose_inner_scope (this_block,
					     insn_scope (body->insn (i)));
	}
      if (! this_block)
	{
	  if (INSN_LOCATION (insn) == UNKNOWN_LOCATION)
	    continue;
	  else
	    this_block = DECL_INITIAL (cfun->decl);
	}

      if (this_block != cur_block)
	{
	  change_scope (insn, cur_block, this_block);
	  cur_block = this_block;
	}
    }

  /* change_scope emits before the insn, not after.  */
  note = emit_note (NOTE_INSN_DELETED);
  change_scope (note, cur_block, DECL_INITIAL (cfun->decl));
  delete_insn (note);

  reorder_blocks ();
}

static const char *some_local_dynamic_name;

/* Locate some local-dynamic symbol still in use by this function
   so that we can print its name in local-dynamic base patterns.
   Return null if there are no local-dynamic references.  */

const char *
get_some_local_dynamic_name ()
{
  subrtx_iterator::array_type array;
  rtx_insn *insn;

  if (some_local_dynamic_name)
    return some_local_dynamic_name;

  for (insn = get_insns (); insn ; insn = NEXT_INSN (insn))
    if (NONDEBUG_INSN_P (insn))
      FOR_EACH_SUBRTX (iter, array, PATTERN (insn), ALL)
	{
	  const_rtx x = *iter;
	  if (GET_CODE (x) == SYMBOL_REF)
	    {
	      if (SYMBOL_REF_TLS_MODEL (x) == TLS_MODEL_LOCAL_DYNAMIC)
		return some_local_dynamic_name = XSTR (x, 0);
	      if (CONSTANT_POOL_ADDRESS_P (x))
		iter.substitute (get_pool_constant (x));
	    }
	}

  return 0;
}

/* Output assembler code for the start of a function,
   and initialize some of the variables in this file
   for the new function.  The label for the function and associated
   assembler pseudo-ops have already been output in `assemble_start_function'.

   FIRST is the first insn of the rtl for the function being compiled.
   FILE is the file to write assembler code to.
   OPTIMIZE_P is nonzero if we should eliminate redundant
     test and compare insns.  */

void
final_start_function (rtx_insn *first, FILE *file,
		      int optimize_p ATTRIBUTE_UNUSED)
{
  block_depth = 0;

  this_is_asm_operands = 0;

  need_profile_function = false;

  last_filename = LOCATION_FILE (prologue_location);
  last_linenum = LOCATION_LINE (prologue_location);
  last_columnnum = LOCATION_COLUMN (prologue_location);
  last_discriminator = discriminator = 0;

  high_block_linenum = high_function_linenum = last_linenum;

  if (flag_sanitize & SANITIZE_ADDRESS)
    asan_function_start ();

  if (!DECL_IGNORED_P (current_function_decl))
    debug_hooks->begin_prologue (last_linenum, last_columnnum, last_filename);

  if (!dwarf2_debug_info_emitted_p (current_function_decl))
    dwarf2out_begin_prologue (0, 0, NULL);

#ifdef LEAF_REG_REMAP
  if (crtl->uses_only_leaf_regs)
    leaf_renumber_regs (first);
#endif

  /* The Sun386i and perhaps other machines don't work right
     if the profiling code comes after the prologue.  */
  if (targetm.profile_before_prologue () && crtl->profile)
    {
      if (targetm.asm_out.function_prologue == default_function_pro_epilogue
	  && targetm.have_prologue ())
	{
	  rtx_insn *insn;
	  for (insn = first; insn; insn = NEXT_INSN (insn))
	    if (!NOTE_P (insn))
	      {
		insn = NULL;
		break;
	      }
	    else if (NOTE_KIND (insn) == NOTE_INSN_BASIC_BLOCK
		     || NOTE_KIND (insn) == NOTE_INSN_FUNCTION_BEG)
	      break;
	    else if (NOTE_KIND (insn) == NOTE_INSN_DELETED
		     || NOTE_KIND (insn) == NOTE_INSN_VAR_LOCATION)
	      continue;
	    else
	      {
		insn = NULL;
		break;
	      }

	  if (insn)
	    need_profile_function = true;
	  else
	    profile_function (file);
	}
      else
	profile_function (file);
    }

  /* If debugging, assign block numbers to all of the blocks in this
     function.  */
  if (write_symbols)
    {
      reemit_insn_block_notes ();
      number_blocks (current_function_decl);
      /* We never actually put out begin/end notes for the top-level
	 block in the function.  But, conceptually, that block is
	 always needed.  */
      TREE_ASM_WRITTEN (DECL_INITIAL (current_function_decl)) = 1;
    }

  if (warn_frame_larger_than
    && get_frame_size () > frame_larger_than_size)
  {
      /* Issue a warning */
      warning (OPT_Wframe_larger_than_,
               "the frame size of %wd bytes is larger than %wd bytes",
               get_frame_size (), frame_larger_than_size);
  }

  /* First output the function prologue: code to set up the stack frame.  */
  targetm.asm_out.function_prologue (file);

  /* If the machine represents the prologue as RTL, the profiling code must
     be emitted when NOTE_INSN_PROLOGUE_END is scanned.  */
  if (! targetm.have_prologue ())
    profile_after_prologue (file);
}

static void
profile_after_prologue (FILE *file ATTRIBUTE_UNUSED)
{
  if (!targetm.profile_before_prologue () && crtl->profile)
    profile_function (file);
}

static void
profile_function (FILE *file ATTRIBUTE_UNUSED)
{
#ifndef NO_PROFILE_COUNTERS
# define NO_PROFILE_COUNTERS	0
#endif
#ifdef ASM_OUTPUT_REG_PUSH
  rtx sval = NULL, chain = NULL;

  if (cfun->returns_struct)
    sval = targetm.calls.struct_value_rtx (TREE_TYPE (current_function_decl),
					   true);
  if (cfun->static_chain_decl)
    chain = targetm.calls.static_chain (current_function_decl, true);
#endif /* ASM_OUTPUT_REG_PUSH */

  if (! NO_PROFILE_COUNTERS)
    {
      int align = MIN (BIGGEST_ALIGNMENT, LONG_TYPE_SIZE);
      switch_to_section (data_section);
      ASM_OUTPUT_ALIGN (file, floor_log2 (align / BITS_PER_UNIT));
      targetm.asm_out.internal_label (file, "LP", current_function_funcdef_no);
      assemble_integer (const0_rtx, LONG_TYPE_SIZE / BITS_PER_UNIT, align, 1);
    }

  switch_to_section (current_function_section ());

#ifdef ASM_OUTPUT_REG_PUSH
  if (sval && REG_P (sval))
    ASM_OUTPUT_REG_PUSH (file, REGNO (sval));
  if (chain && REG_P (chain))
    ASM_OUTPUT_REG_PUSH (file, REGNO (chain));
#endif

  FUNCTION_PROFILER (file, current_function_funcdef_no);

#ifdef ASM_OUTPUT_REG_PUSH
  if (chain && REG_P (chain))
    ASM_OUTPUT_REG_POP (file, REGNO (chain));
  if (sval && REG_P (sval))
    ASM_OUTPUT_REG_POP (file, REGNO (sval));
#endif
}

/* Output assembler code for the end of a function.
   For clarity, args are same as those of `final_start_function'
   even though not all of them are needed.  */

void
final_end_function (void)
{
  app_disable ();

  if (!DECL_IGNORED_P (current_function_decl))
    debug_hooks->end_function (high_function_linenum);

  /* Finally, output the function epilogue:
     code to restore the stack frame and return to the caller.  */
  targetm.asm_out.function_epilogue (asm_out_file);

  /* And debug output.  */
  if (!DECL_IGNORED_P (current_function_decl))
    debug_hooks->end_epilogue (last_linenum, last_filename);

  if (!dwarf2_debug_info_emitted_p (current_function_decl)
      && dwarf2out_do_frame ())
    dwarf2out_end_epilogue (last_linenum, last_filename);

  some_local_dynamic_name = 0;
}


/* Dumper helper for basic block information. FILE is the assembly
   output file, and INSN is the instruction being emitted.  */

static void
dump_basic_block_info (FILE *file, rtx_insn *insn, basic_block *start_to_bb,
                       basic_block *end_to_bb, int bb_map_size, int *bb_seqn)
{
  basic_block bb;

  if (!flag_debug_asm)
    return;

  if (INSN_UID (insn) < bb_map_size
      && (bb = start_to_bb[INSN_UID (insn)]) != NULL)
    {
      edge e;
      edge_iterator ei;

      fprintf (file, "%s BLOCK %d", ASM_COMMENT_START, bb->index);
      if (bb->frequency)
        fprintf (file, " freq:%d", bb->frequency);
      if (bb->count.initialized_p ())
	{
          fprintf (file, ", count:");
	  bb->count.dump (file);
	}
      fprintf (file, " seq:%d", (*bb_seqn)++);
      fprintf (file, "\n%s PRED:", ASM_COMMENT_START);
      FOR_EACH_EDGE (e, ei, bb->preds)
        {
          dump_edge_info (file, e, TDF_DETAILS, 0);
        }
      fprintf (file, "\n");
    }
  if (INSN_UID (insn) < bb_map_size
      && (bb = end_to_bb[INSN_UID (insn)]) != NULL)
    {
      edge e;
      edge_iterator ei;

      fprintf (asm_out_file, "%s SUCC:", ASM_COMMENT_START);
      FOR_EACH_EDGE (e, ei, bb->succs)
       {
         dump_edge_info (asm_out_file, e, TDF_DETAILS, 1);
       }
      fprintf (file, "\n");
    }
}

/* Output assembler code for some insns: all or part of a function.
   For description of args, see `final_start_function', above.  */

void
final (rtx_insn *first, FILE *file, int optimize_p)
{
  rtx_insn *insn, *next;
  int seen = 0;

  /* Used for -dA dump.  */
  basic_block *start_to_bb = NULL;
  basic_block *end_to_bb = NULL;
  int bb_map_size = 0;
  int bb_seqn = 0;

  last_ignored_compare = 0;

  if (HAVE_cc0)
    for (insn = first; insn; insn = NEXT_INSN (insn))
      {
	/* If CC tracking across branches is enabled, record the insn which
	   jumps to each branch only reached from one place.  */
	if (optimize_p && JUMP_P (insn))
	  {
	    rtx lab = JUMP_LABEL (insn);
	    if (lab && LABEL_P (lab) && LABEL_NUSES (lab) == 1)
	      {
		LABEL_REFS (lab) = insn;
	      }
	  }
      }

  init_recog ();

  CC_STATUS_INIT;

  if (flag_debug_asm)
    {
      basic_block bb;

      bb_map_size = get_max_uid () + 1;
      start_to_bb = XCNEWVEC (basic_block, bb_map_size);
      end_to_bb = XCNEWVEC (basic_block, bb_map_size);

      /* There is no cfg for a thunk.  */
      if (!cfun->is_thunk)
	FOR_EACH_BB_REVERSE_FN (bb, cfun)
	  {
	    start_to_bb[INSN_UID (BB_HEAD (bb))] = bb;
	    end_to_bb[INSN_UID (BB_END (bb))] = bb;
	  }
    }

  /* Output the insns.  */
  for (insn = first; insn;)
    {
      if (HAVE_ATTR_length)
	{
	  if ((unsigned) INSN_UID (insn) >= INSN_ADDRESSES_SIZE ())
	    {
	      /* This can be triggered by bugs elsewhere in the compiler if
		 new insns are created after init_insn_lengths is called.  */
	      gcc_assert (NOTE_P (insn));
	      insn_current_address = -1;
	    }
	  else
	    insn_current_address = INSN_ADDRESSES (INSN_UID (insn));
	}

      dump_basic_block_info (file, insn, start_to_bb, end_to_bb,
                             bb_map_size, &bb_seqn);
      insn = final_scan_insn (insn, file, optimize_p, 0, &seen);
    }

  if (flag_debug_asm)
    {
      free (start_to_bb);
      free (end_to_bb);
    }

  /* Remove CFI notes, to avoid compare-debug failures.  */
  for (insn = first; insn; insn = next)
    {
      next = NEXT_INSN (insn);
      if (NOTE_P (insn)
	  && (NOTE_KIND (insn) == NOTE_INSN_CFI
	      || NOTE_KIND (insn) == NOTE_INSN_CFI_LABEL))
	delete_insn (insn);
    }
}

const char *
get_insn_template (int code, rtx insn)
{
  switch (insn_data[code].output_format)
    {
    case INSN_OUTPUT_FORMAT_SINGLE:
      return insn_data[code].output.single;
    case INSN_OUTPUT_FORMAT_MULTI:
      return insn_data[code].output.multi[which_alternative];
    case INSN_OUTPUT_FORMAT_FUNCTION:
      gcc_assert (insn);
      return (*insn_data[code].output.function) (recog_data.operand,
						 as_a <rtx_insn *> (insn));

    default:
      gcc_unreachable ();
    }
}

/* Emit the appropriate declaration for an alternate-entry-point
   symbol represented by INSN, to FILE.  INSN is a CODE_LABEL with
   LABEL_KIND != LABEL_NORMAL.

   The case fall-through in this function is intentional.  */
static void
output_alternate_entry_point (FILE *file, rtx_insn *insn)
{
  const char *name = LABEL_NAME (insn);

  switch (LABEL_KIND (insn))
    {
    case LABEL_WEAK_ENTRY:
#ifdef ASM_WEAKEN_LABEL
      ASM_WEAKEN_LABEL (file, name);
      gcc_fallthrough ();
#endif
    case LABEL_GLOBAL_ENTRY:
      targetm.asm_out.globalize_label (file, name);
      gcc_fallthrough ();
    case LABEL_STATIC_ENTRY:
#ifdef ASM_OUTPUT_TYPE_DIRECTIVE
      ASM_OUTPUT_TYPE_DIRECTIVE (file, name, "function");
#endif
      ASM_OUTPUT_LABEL (file, name);
      break;

    case LABEL_NORMAL:
    default:
      gcc_unreachable ();
    }
}

/* Given a CALL_INSN, find and return the nested CALL. */
static rtx
call_from_call_insn (rtx_call_insn *insn)
{
  rtx x;
  gcc_assert (CALL_P (insn));
  x = PATTERN (insn);

  while (GET_CODE (x) != CALL)
    {
      switch (GET_CODE (x))
	{
	default:
	  gcc_unreachable ();
	case COND_EXEC:
	  x = COND_EXEC_CODE (x);
	  break;
	case PARALLEL:
	  x = XVECEXP (x, 0, 0);
	  break;
	case SET:
	  x = XEXP (x, 1);
	  break;
	}
    }
  return x;
}

/* Print a comment into the asm showing FILENAME, LINENUM, and the
   corresponding source line, if available.  */

static void
asm_show_source (const char *filename, int linenum)
{
  if (!filename)
    return;

  int line_size;
  const char *line = location_get_source_line (filename, linenum, &line_size);
  if (!line)
    return;

  fprintf (asm_out_file, "%s %s:%i: ", ASM_COMMENT_START, filename, linenum);
  /* "line" is not 0-terminated, so we must use line_size.  */
  fwrite (line, 1, line_size, asm_out_file);
  fputc ('\n', asm_out_file);
}

/* The final scan for one insn, INSN.
   Args are same as in `final', except that INSN
   is the insn being scanned.
   Value returned is the next insn to be scanned.

   NOPEEPHOLES is the flag to disallow peephole processing (currently
   used for within delayed branch sequence output).

   SEEN is used to track the end of the prologue, for emitting
   debug information.  We force the emission of a line note after
   both NOTE_INSN_PROLOGUE_END and NOTE_INSN_FUNCTION_BEG.  */

rtx_insn *
final_scan_insn (rtx_insn *insn, FILE *file, int optimize_p ATTRIBUTE_UNUSED,
		 int nopeepholes ATTRIBUTE_UNUSED, int *seen)
{
#if HAVE_cc0
  rtx set;
#endif
  rtx_insn *next;
  rtx_jump_table_data *table;

  insn_counter++;

  /* Ignore deleted insns.  These can occur when we split insns (due to a
     template of "#") while not optimizing.  */
  if (insn->deleted ())
    return NEXT_INSN (insn);

  switch (GET_CODE (insn))
    {
    case NOTE:
      switch (NOTE_KIND (insn))
	{
	case NOTE_INSN_DELETED:
	case NOTE_INSN_UPDATE_SJLJ_CONTEXT:
	  break;

	case NOTE_INSN_SWITCH_TEXT_SECTIONS:
	  in_cold_section_p = !in_cold_section_p;

	  if (dwarf2out_do_frame ())
	    dwarf2out_switch_text_section ();
	  else if (!DECL_IGNORED_P (current_function_decl))
	    debug_hooks->switch_text_section ();

	  switch_to_section (current_function_section ());
	  targetm.asm_out.function_switched_text_sections (asm_out_file,
							   current_function_decl,
							   in_cold_section_p);
	  /* Emit a label for the split cold section.  Form label name by
	     suffixing "cold" to the original function's name.  */
	  if (in_cold_section_p)
	    {
	      cold_function_name
		= clone_function_name (current_function_decl, "cold");
#ifdef ASM_DECLARE_COLD_FUNCTION_NAME
	      ASM_DECLARE_COLD_FUNCTION_NAME (asm_out_file,
					      IDENTIFIER_POINTER
					          (cold_function_name),
					      current_function_decl);
#else
	      ASM_OUTPUT_LABEL (asm_out_file,
				IDENTIFIER_POINTER (cold_function_name));
#endif
	    }
	  break;

	case NOTE_INSN_BASIC_BLOCK:
	  if (need_profile_function)
	    {
	      profile_function (asm_out_file);
	      need_profile_function = false;
	    }

	  if (targetm.asm_out.unwind_emit)
	    targetm.asm_out.unwind_emit (asm_out_file, insn);

          discriminator = NOTE_BASIC_BLOCK (insn)->discriminator;

	  break;

	case NOTE_INSN_EH_REGION_BEG:
	  ASM_OUTPUT_DEBUG_LABEL (asm_out_file, "LEHB",
				  NOTE_EH_HANDLER (insn));
	  break;

	case NOTE_INSN_EH_REGION_END:
	  ASM_OUTPUT_DEBUG_LABEL (asm_out_file, "LEHE",
				  NOTE_EH_HANDLER (insn));
	  break;

	case NOTE_INSN_PROLOGUE_END:
	  targetm.asm_out.function_end_prologue (file);
	  profile_after_prologue (file);

	  if ((*seen & (SEEN_EMITTED | SEEN_NOTE)) == SEEN_NOTE)
	    {
	      *seen |= SEEN_EMITTED;
	      force_source_line = true;
	    }
	  else
	    *seen |= SEEN_NOTE;

	  break;

	case NOTE_INSN_EPILOGUE_BEG:
          if (!DECL_IGNORED_P (current_function_decl))
            (*debug_hooks->begin_epilogue) (last_linenum, last_filename);
	  targetm.asm_out.function_begin_epilogue (file);
	  break;

	case NOTE_INSN_CFI:
	  dwarf2out_emit_cfi (NOTE_CFI (insn));
	  break;

	case NOTE_INSN_CFI_LABEL:
	  ASM_OUTPUT_DEBUG_LABEL (asm_out_file, "LCFI",
				  NOTE_LABEL_NUMBER (insn));
	  break;

	case NOTE_INSN_FUNCTION_BEG:
	  if (need_profile_function)
	    {
	      profile_function (asm_out_file);
	      need_profile_function = false;
	    }

	  app_disable ();
	  if (!DECL_IGNORED_P (current_function_decl))
	    debug_hooks->end_prologue (last_linenum, last_filename);

	  if ((*seen & (SEEN_EMITTED | SEEN_NOTE)) == SEEN_NOTE)
	    {
	      *seen |= SEEN_EMITTED;
	      force_source_line = true;
	    }
	  else
	    *seen |= SEEN_NOTE;

	  break;

	case NOTE_INSN_BLOCK_BEG:
	  if (debug_info_level == DINFO_LEVEL_NORMAL
	      || debug_info_level == DINFO_LEVEL_VERBOSE
	      || write_symbols == DWARF2_DEBUG
	      || write_symbols == VMS_AND_DWARF2_DEBUG
	      || write_symbols == VMS_DEBUG)
	    {
	      int n = BLOCK_NUMBER (NOTE_BLOCK (insn));

	      app_disable ();
	      ++block_depth;
	      high_block_linenum = last_linenum;

	      /* Output debugging info about the symbol-block beginning.  */
	      if (!DECL_IGNORED_P (current_function_decl))
		debug_hooks->begin_block (last_linenum, n);

	      /* Mark this block as output.  */
	      TREE_ASM_WRITTEN (NOTE_BLOCK (insn)) = 1;
	      BLOCK_IN_COLD_SECTION_P (NOTE_BLOCK (insn)) = in_cold_section_p;
	    }
	  if (write_symbols == DBX_DEBUG
	      || write_symbols == SDB_DEBUG)
	    {
	      location_t *locus_ptr
		= block_nonartificial_location (NOTE_BLOCK (insn));

	      if (locus_ptr != NULL)
		{
		  override_filename = LOCATION_FILE (*locus_ptr);
		  override_linenum = LOCATION_LINE (*locus_ptr);
		  override_columnnum = LOCATION_COLUMN (*locus_ptr);
		}
	    }
	  break;

	case NOTE_INSN_BLOCK_END:
	  if (debug_info_level == DINFO_LEVEL_NORMAL
	      || debug_info_level == DINFO_LEVEL_VERBOSE
	      || write_symbols == DWARF2_DEBUG
	      || write_symbols == VMS_AND_DWARF2_DEBUG
	      || write_symbols == VMS_DEBUG)
	    {
	      int n = BLOCK_NUMBER (NOTE_BLOCK (insn));

	      app_disable ();

	      /* End of a symbol-block.  */
	      --block_depth;
	      gcc_assert (block_depth >= 0);

	      if (!DECL_IGNORED_P (current_function_decl))
		debug_hooks->end_block (high_block_linenum, n);
	      gcc_assert (BLOCK_IN_COLD_SECTION_P (NOTE_BLOCK (insn))
			  == in_cold_section_p);
	    }
	  if (write_symbols == DBX_DEBUG
	      || write_symbols == SDB_DEBUG)
	    {
	      tree outer_block = BLOCK_SUPERCONTEXT (NOTE_BLOCK (insn));
	      location_t *locus_ptr
		= block_nonartificial_location (outer_block);

	      if (locus_ptr != NULL)
		{
		  override_filename = LOCATION_FILE (*locus_ptr);
		  override_linenum = LOCATION_LINE (*locus_ptr);
		  override_columnnum = LOCATION_COLUMN (*locus_ptr);
		}
	      else
		{
		  override_filename = NULL;
		  override_linenum = 0;
		  override_columnnum = 0;
		}
	    }
	  break;

	case NOTE_INSN_DELETED_LABEL:
	  /* Emit the label.  We may have deleted the CODE_LABEL because
	     the label could be proved to be unreachable, though still
	     referenced (in the form of having its address taken.  */
	  ASM_OUTPUT_DEBUG_LABEL (file, "L", CODE_LABEL_NUMBER (insn));
	  break;

	case NOTE_INSN_DELETED_DEBUG_LABEL:
	  /* Similarly, but need to use different namespace for it.  */
	  if (CODE_LABEL_NUMBER (insn) != -1)
	    ASM_OUTPUT_DEBUG_LABEL (file, "LDL", CODE_LABEL_NUMBER (insn));
	  break;

	case NOTE_INSN_VAR_LOCATION:
	case NOTE_INSN_CALL_ARG_LOCATION:
	  if (!DECL_IGNORED_P (current_function_decl))
	    debug_hooks->var_location (insn);
	  break;

	default:
	  gcc_unreachable ();
	  break;
	}
      break;

    case BARRIER:
      break;

    case CODE_LABEL:
      /* The target port might emit labels in the output function for
	 some insn, e.g. sh.c output_branchy_insn.  */
      if (CODE_LABEL_NUMBER (insn) <= max_labelno)
	{
	  int align = LABEL_TO_ALIGNMENT (insn);
#ifdef ASM_OUTPUT_MAX_SKIP_ALIGN
	  int max_skip = LABEL_TO_MAX_SKIP (insn);
#endif

	  if (align && NEXT_INSN (insn))
	    {
#ifdef ASM_OUTPUT_MAX_SKIP_ALIGN
	      ASM_OUTPUT_MAX_SKIP_ALIGN (file, align, max_skip);
#else
#ifdef ASM_OUTPUT_ALIGN_WITH_NOP
              ASM_OUTPUT_ALIGN_WITH_NOP (file, align);
#else
	      ASM_OUTPUT_ALIGN (file, align);
#endif
#endif
	    }
	}
      CC_STATUS_INIT;

      if (!DECL_IGNORED_P (current_function_decl) && LABEL_NAME (insn))
	debug_hooks->label (as_a <rtx_code_label *> (insn));

      app_disable ();

      /* If this label is followed by a jump-table, make sure we put
	 the label in the read-only section.  Also possibly write the
	 label and jump table together.  */
      table = jump_table_for_label (as_a <rtx_code_label *> (insn));
      if (table)
	{
#if defined(ASM_OUTPUT_ADDR_VEC) || defined(ASM_OUTPUT_ADDR_DIFF_VEC)
	  /* In this case, the case vector is being moved by the
	     target, so don't output the label at all.  Leave that
	     to the back end macros.  */
#else
	  if (! JUMP_TABLES_IN_TEXT_SECTION)
	    {
	      int log_align;

	      switch_to_section (targetm.asm_out.function_rodata_section
				 (current_function_decl));

#ifdef ADDR_VEC_ALIGN
	      log_align = ADDR_VEC_ALIGN (table);
#else
	      log_align = exact_log2 (BIGGEST_ALIGNMENT / BITS_PER_UNIT);
#endif
	      ASM_OUTPUT_ALIGN (file, log_align);
	    }
	  else
	    switch_to_section (current_function_section ());

#ifdef ASM_OUTPUT_CASE_LABEL
	  ASM_OUTPUT_CASE_LABEL (file, "L", CODE_LABEL_NUMBER (insn), table);
#else
	  targetm.asm_out.internal_label (file, "L", CODE_LABEL_NUMBER (insn));
#endif
#endif
	  break;
	}
      if (LABEL_ALT_ENTRY_P (insn))
	output_alternate_entry_point (file, insn);
      else
	targetm.asm_out.internal_label (file, "L", CODE_LABEL_NUMBER (insn));
      break;

    default:
      {
	rtx body = PATTERN (insn);
	int insn_code_number;
	const char *templ;
	bool is_stmt;

	/* Reset this early so it is correct for ASM statements.  */
	current_insn_predicate = NULL_RTX;

	/* An INSN, JUMP_INSN or CALL_INSN.
	   First check for special kinds that recog doesn't recognize.  */

	if (GET_CODE (body) == USE /* These are just declarations.  */
	    || GET_CODE (body) == CLOBBER)
	  break;

#if HAVE_cc0
	{
	  /* If there is a REG_CC_SETTER note on this insn, it means that
	     the setting of the condition code was done in the delay slot
	     of the insn that branched here.  So recover the cc status
	     from the insn that set it.  */

	  rtx note = find_reg_note (insn, REG_CC_SETTER, NULL_RTX);
	  if (note)
	    {
	      rtx_insn *other = as_a <rtx_insn *> (XEXP (note, 0));
	      NOTICE_UPDATE_CC (PATTERN (other), other);
	      cc_prev_status = cc_status;
	    }
	}
#endif

	/* Detect insns that are really jump-tables
	   and output them as such.  */

        if (JUMP_TABLE_DATA_P (insn))
	  {
#if !(defined(ASM_OUTPUT_ADDR_VEC) || defined(ASM_OUTPUT_ADDR_DIFF_VEC))
	    int vlen, idx;
#endif

	    if (! JUMP_TABLES_IN_TEXT_SECTION)
	      switch_to_section (targetm.asm_out.function_rodata_section
				 (current_function_decl));
	    else
	      switch_to_section (current_function_section ());

	    app_disable ();

#if defined(ASM_OUTPUT_ADDR_VEC) || defined(ASM_OUTPUT_ADDR_DIFF_VEC)
	    if (GET_CODE (body) == ADDR_VEC)
	      {
#ifdef ASM_OUTPUT_ADDR_VEC
		ASM_OUTPUT_ADDR_VEC (PREV_INSN (insn), body);
#else
		gcc_unreachable ();
#endif
	      }
	    else
	      {
#ifdef ASM_OUTPUT_ADDR_DIFF_VEC
		ASM_OUTPUT_ADDR_DIFF_VEC (PREV_INSN (insn), body);
#else
		gcc_unreachable ();
#endif
	      }
#else
	    vlen = XVECLEN (body, GET_CODE (body) == ADDR_DIFF_VEC);
	    for (idx = 0; idx < vlen; idx++)
	      {
		if (GET_CODE (body) == ADDR_VEC)
		  {
#ifdef ASM_OUTPUT_ADDR_VEC_ELT
		    ASM_OUTPUT_ADDR_VEC_ELT
		      (file, CODE_LABEL_NUMBER (XEXP (XVECEXP (body, 0, idx), 0)));
#else
		    gcc_unreachable ();
#endif
		  }
		else
		  {
#ifdef ASM_OUTPUT_ADDR_DIFF_ELT
		    ASM_OUTPUT_ADDR_DIFF_ELT
		      (file,
		       body,
		       CODE_LABEL_NUMBER (XEXP (XVECEXP (body, 1, idx), 0)),
		       CODE_LABEL_NUMBER (XEXP (XEXP (body, 0), 0)));
#else
		    gcc_unreachable ();
#endif
		  }
	      }
#ifdef ASM_OUTPUT_CASE_END
	    ASM_OUTPUT_CASE_END (file,
				 CODE_LABEL_NUMBER (PREV_INSN (insn)),
				 insn);
#endif
#endif

	    switch_to_section (current_function_section ());

	    break;
	  }
	/* Output this line note if it is the first or the last line
	   note in a row.  */
	if (!DECL_IGNORED_P (current_function_decl)
	    && notice_source_line (insn, &is_stmt))
	  {
	    if (flag_verbose_asm)
	      asm_show_source (last_filename, last_linenum);
	    (*debug_hooks->source_line) (last_linenum, last_columnnum,
					 last_filename, last_discriminator,
					 is_stmt);
	  }

	if (GET_CODE (body) == PARALLEL
	    && GET_CODE (XVECEXP (body, 0, 0)) == ASM_INPUT)
	  body = XVECEXP (body, 0, 0);

	if (GET_CODE (body) == ASM_INPUT)
	  {
	    const char *string = XSTR (body, 0);

	    /* There's no telling what that did to the condition codes.  */
	    CC_STATUS_INIT;

	    if (string[0])
	      {
		expanded_location loc;

		app_enable ();
		loc = expand_location (ASM_INPUT_SOURCE_LOCATION (body));
		if (*loc.file && loc.line)
		  fprintf (asm_out_file, "%s %i \"%s\" 1\n",
			   ASM_COMMENT_START, loc.line, loc.file);
		fprintf (asm_out_file, "\t%s\n", string);
#if HAVE_AS_LINE_ZERO
		if (*loc.file && loc.line)
		  fprintf (asm_out_file, "%s 0 \"\" 2\n", ASM_COMMENT_START);
#endif
	      }
	    break;
	  }

	/* Detect `asm' construct with operands.  */
	if (asm_noperands (body) >= 0)
	  {
	    unsigned int noperands = asm_noperands (body);
	    rtx *ops = XALLOCAVEC (rtx, noperands);
	    const char *string;
	    location_t loc;
	    expanded_location expanded;

	    /* There's no telling what that did to the condition codes.  */
	    CC_STATUS_INIT;

	    /* Get out the operand values.  */
	    string = decode_asm_operands (body, ops, NULL, NULL, NULL, &loc);
	    /* Inhibit dying on what would otherwise be compiler bugs.  */
	    insn_noperands = noperands;
	    this_is_asm_operands = insn;
	    expanded = expand_location (loc);

#ifdef FINAL_PRESCAN_INSN
	    FINAL_PRESCAN_INSN (insn, ops, insn_noperands);
#endif

	    /* Output the insn using them.  */
	    if (string[0])
	      {
		app_enable ();
		if (expanded.file && expanded.line)
		  fprintf (asm_out_file, "%s %i \"%s\" 1\n",
			   ASM_COMMENT_START, expanded.line, expanded.file);
	        output_asm_insn (string, ops);
#if HAVE_AS_LINE_ZERO
		if (expanded.file && expanded.line)
		  fprintf (asm_out_file, "%s 0 \"\" 2\n", ASM_COMMENT_START);
#endif
	      }

	    if (targetm.asm_out.final_postscan_insn)
	      targetm.asm_out.final_postscan_insn (file, insn, ops,
						   insn_noperands);

	    this_is_asm_operands = 0;
	    break;
	  }

	app_disable ();

	if (rtx_sequence *seq = dyn_cast <rtx_sequence *> (body))
	  {
	    /* A delayed-branch sequence */
	    int i;

	    final_sequence = seq;

	    /* The first insn in this SEQUENCE might be a JUMP_INSN that will
	       force the restoration of a comparison that was previously
	       thought unnecessary.  If that happens, cancel this sequence
	       and cause that insn to be restored.  */

	    next = final_scan_insn (seq->insn (0), file, 0, 1, seen);
	    if (next != seq->insn (1))
	      {
		final_sequence = 0;
		return next;
	      }

	    for (i = 1; i < seq->len (); i++)
	      {
		rtx_insn *insn = seq->insn (i);
		rtx_insn *next = NEXT_INSN (insn);
		/* We loop in case any instruction in a delay slot gets
		   split.  */
		do
		  insn = final_scan_insn (insn, file, 0, 1, seen);
		while (insn != next);
	      }
#ifdef DBR_OUTPUT_SEQEND
	    DBR_OUTPUT_SEQEND (file);
#endif
	    final_sequence = 0;

	    /* If the insn requiring the delay slot was a CALL_INSN, the
	       insns in the delay slot are actually executed before the
	       called function.  Hence we don't preserve any CC-setting
	       actions in these insns and the CC must be marked as being
	       clobbered by the function.  */
	    if (CALL_P (seq->insn (0)))
	      {
		CC_STATUS_INIT;
	      }
	    break;
	  }

	/* We have a real machine instruction as rtl.  */

	body = PATTERN (insn);

#if HAVE_cc0
	set = single_set (insn);

	/* Check for redundant test and compare instructions
	   (when the condition codes are already set up as desired).
	   This is done only when optimizing; if not optimizing,
	   it should be possible for the user to alter a variable
	   with the debugger in between statements
	   and the next statement should reexamine the variable
	   to compute the condition codes.  */

	if (optimize_p)
	  {
	    if (set
		&& GET_CODE (SET_DEST (set)) == CC0
		&& insn != last_ignored_compare)
	      {
		rtx src1, src2;
		if (GET_CODE (SET_SRC (set)) == SUBREG)
		  SET_SRC (set) = alter_subreg (&SET_SRC (set), true);

		src1 = SET_SRC (set);
		src2 = NULL_RTX;
		if (GET_CODE (SET_SRC (set)) == COMPARE)
		  {
		    if (GET_CODE (XEXP (SET_SRC (set), 0)) == SUBREG)
		      XEXP (SET_SRC (set), 0)
			= alter_subreg (&XEXP (SET_SRC (set), 0), true);
		    if (GET_CODE (XEXP (SET_SRC (set), 1)) == SUBREG)
		      XEXP (SET_SRC (set), 1)
			= alter_subreg (&XEXP (SET_SRC (set), 1), true);
		    if (XEXP (SET_SRC (set), 1)
			== CONST0_RTX (GET_MODE (XEXP (SET_SRC (set), 0))))
		      src2 = XEXP (SET_SRC (set), 0);
		  }
		if ((cc_status.value1 != 0
		     && rtx_equal_p (src1, cc_status.value1))
		    || (cc_status.value2 != 0
			&& rtx_equal_p (src1, cc_status.value2))
		    || (src2 != 0 && cc_status.value1 != 0
		        && rtx_equal_p (src2, cc_status.value1))
		    || (src2 != 0 && cc_status.value2 != 0
			&& rtx_equal_p (src2, cc_status.value2)))
		  {
		    /* Don't delete insn if it has an addressing side-effect.  */
		    if (! FIND_REG_INC_NOTE (insn, NULL_RTX)
			/* or if anything in it is volatile.  */
			&& ! volatile_refs_p (PATTERN (insn)))
		      {
			/* We don't really delete the insn; just ignore it.  */
			last_ignored_compare = insn;
			break;
		      }
		  }
	      }
	  }

	/* If this is a conditional branch, maybe modify it
	   if the cc's are in a nonstandard state
	   so that it accomplishes the same thing that it would
	   do straightforwardly if the cc's were set up normally.  */

	if (cc_status.flags != 0
	    && JUMP_P (insn)
	    && GET_CODE (body) == SET
	    && SET_DEST (body) == pc_rtx
	    && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE
	    && COMPARISON_P (XEXP (SET_SRC (body), 0))
	    && XEXP (XEXP (SET_SRC (body), 0), 0) == cc0_rtx)
	  {
	    /* This function may alter the contents of its argument
	       and clear some of the cc_status.flags bits.
	       It may also return 1 meaning condition now always true
	       or -1 meaning condition now always false
	       or 2 meaning condition nontrivial but altered.  */
	    int result = alter_cond (XEXP (SET_SRC (body), 0));
	    /* If condition now has fixed value, replace the IF_THEN_ELSE
	       with its then-operand or its else-operand.  */
	    if (result == 1)
	      SET_SRC (body) = XEXP (SET_SRC (body), 1);
	    if (result == -1)
	      SET_SRC (body) = XEXP (SET_SRC (body), 2);

	    /* The jump is now either unconditional or a no-op.
	       If it has become a no-op, don't try to output it.
	       (It would not be recognized.)  */
	    if (SET_SRC (body) == pc_rtx)
	      {
	        delete_insn (insn);
		break;
	      }
	    else if (ANY_RETURN_P (SET_SRC (body)))
	      /* Replace (set (pc) (return)) with (return).  */
	      PATTERN (insn) = body = SET_SRC (body);

	    /* Rerecognize the instruction if it has changed.  */
	    if (result != 0)
	      INSN_CODE (insn) = -1;
	  }

	/* If this is a conditional trap, maybe modify it if the cc's
	   are in a nonstandard state so that it accomplishes the same
	   thing that it would do straightforwardly if the cc's were
	   set up normally.  */
	if (cc_status.flags != 0
	    && NONJUMP_INSN_P (insn)
	    && GET_CODE (body) == TRAP_IF
	    && COMPARISON_P (TRAP_CONDITION (body))
	    && XEXP (TRAP_CONDITION (body), 0) == cc0_rtx)
	  {
	    /* This function may alter the contents of its argument
	       and clear some of the cc_status.flags bits.
	       It may also return 1 meaning condition now always true
	       or -1 meaning condition now always false
	       or 2 meaning condition nontrivial but altered.  */
	    int result = alter_cond (TRAP_CONDITION (body));

	    /* If TRAP_CONDITION has become always false, delete the
	       instruction.  */
	    if (result == -1)
	      {
		delete_insn (insn);
		break;
	      }

	    /* If TRAP_CONDITION has become always true, replace
	       TRAP_CONDITION with const_true_rtx.  */
	    if (result == 1)
	      TRAP_CONDITION (body) = const_true_rtx;

	    /* Rerecognize the instruction if it has changed.  */
	    if (result != 0)
	      INSN_CODE (insn) = -1;
	  }

	/* Make same adjustments to instructions that examine the
	   condition codes without jumping and instructions that
	   handle conditional moves (if this machine has either one).  */

	if (cc_status.flags != 0
	    && set != 0)
	  {
	    rtx cond_rtx, then_rtx, else_rtx;

	    if (!JUMP_P (insn)
		&& GET_CODE (SET_SRC (set)) == IF_THEN_ELSE)
	      {
		cond_rtx = XEXP (SET_SRC (set), 0);
		then_rtx = XEXP (SET_SRC (set), 1);
		else_rtx = XEXP (SET_SRC (set), 2);
	      }
	    else
	      {
		cond_rtx = SET_SRC (set);
		then_rtx = const_true_rtx;
		else_rtx = const0_rtx;
	      }

	    if (COMPARISON_P (cond_rtx)
		&& XEXP (cond_rtx, 0) == cc0_rtx)
	      {
		int result;
		result = alter_cond (cond_rtx);
		if (result == 1)
		  validate_change (insn, &SET_SRC (set), then_rtx, 0);
		else if (result == -1)
		  validate_change (insn, &SET_SRC (set), else_rtx, 0);
		else if (result == 2)
		  INSN_CODE (insn) = -1;
		if (SET_DEST (set) == SET_SRC (set))
		  delete_insn (insn);
	      }
	  }

#endif

	/* Do machine-specific peephole optimizations if desired.  */

	if (HAVE_peephole && optimize_p && !flag_no_peephole && !nopeepholes)
	  {
	    rtx_insn *next = peephole (insn);
	    /* When peepholing, if there were notes within the peephole,
	       emit them before the peephole.  */
	    if (next != 0 && next != NEXT_INSN (insn))
	      {
		rtx_insn *note, *prev = PREV_INSN (insn);

		for (note = NEXT_INSN (insn); note != next;
		     note = NEXT_INSN (note))
		  final_scan_insn (note, file, optimize_p, nopeepholes, seen);

		/* Put the notes in the proper position for a later
		   rescan.  For example, the SH target can do this
		   when generating a far jump in a delayed branch
		   sequence.  */
		note = NEXT_INSN (insn);
		SET_PREV_INSN (note) = prev;
		SET_NEXT_INSN (prev) = note;
		SET_NEXT_INSN (PREV_INSN (next)) = insn;
		SET_PREV_INSN (insn) = PREV_INSN (next);
		SET_NEXT_INSN (insn) = next;
		SET_PREV_INSN (next) = insn;
	      }

	    /* PEEPHOLE might have changed this.  */
	    body = PATTERN (insn);
	  }

	/* Try to recognize the instruction.
	   If successful, verify that the operands satisfy the
	   constraints for the instruction.  Crash if they don't,
	   since `reload' should have changed them so that they do.  */

	insn_code_number = recog_memoized (insn);
	cleanup_subreg_operands (insn);

	/* Dump the insn in the assembly for debugging (-dAP).
	   If the final dump is requested as slim RTL, dump slim
	   RTL to the assembly file also.  */
	if (flag_dump_rtl_in_asm)
	  {
	    print_rtx_head = ASM_COMMENT_START;
	    if (! (dump_flags & TDF_SLIM))
	      print_rtl_single (asm_out_file, insn);
	    else
	      dump_insn_slim (asm_out_file, insn);
	    print_rtx_head = "";
	  }

	if (! constrain_operands_cached (insn, 1))
	  fatal_insn_not_found (insn);

	/* Some target machines need to prescan each insn before
	   it is output.  */

#ifdef FINAL_PRESCAN_INSN
	FINAL_PRESCAN_INSN (insn, recog_data.operand, recog_data.n_operands);
#endif

	if (targetm.have_conditional_execution ()
	    && GET_CODE (PATTERN (insn)) == COND_EXEC)
	  current_insn_predicate = COND_EXEC_TEST (PATTERN (insn));

#if HAVE_cc0
	cc_prev_status = cc_status;

	/* Update `cc_status' for this instruction.
	   The instruction's output routine may change it further.
	   If the output routine for a jump insn needs to depend
	   on the cc status, it should look at cc_prev_status.  */

	NOTICE_UPDATE_CC (body, insn);
#endif

	current_output_insn = debug_insn = insn;

	/* Find the proper template for this insn.  */
	templ = get_insn_template (insn_code_number, insn);

	/* If the C code returns 0, it means that it is a jump insn
	   which follows a deleted test insn, and that test insn
	   needs to be reinserted.  */
	if (templ == 0)
	  {
	    rtx_insn *prev;

	    gcc_assert (prev_nonnote_insn (insn) == last_ignored_compare);

	    /* We have already processed the notes between the setter and
	       the user.  Make sure we don't process them again, this is
	       particularly important if one of the notes is a block
	       scope note or an EH note.  */
	    for (prev = insn;
		 prev != last_ignored_compare;
		 prev = PREV_INSN (prev))
	      {
		if (NOTE_P (prev))
		  delete_insn (prev);	/* Use delete_note.  */
	      }

	    return prev;
	  }

	/* If the template is the string "#", it means that this insn must
	   be split.  */
	if (templ[0] == '#' && templ[1] == '\0')
	  {
	    rtx_insn *new_rtx = try_split (body, insn, 0);

	    /* If we didn't split the insn, go away.  */
	    if (new_rtx == insn && PATTERN (new_rtx) == body)
	      fatal_insn ("could not split insn", insn);

	    /* If we have a length attribute, this instruction should have
	       been split in shorten_branches, to ensure that we would have
	       valid length info for the splitees.  */
	    gcc_assert (!HAVE_ATTR_length);

	    return new_rtx;
	  }

	/* ??? This will put the directives in the wrong place if
	   get_insn_template outputs assembly directly.  However calling it
	   before get_insn_template breaks if the insns is split.  */
	if (targetm.asm_out.unwind_emit_before_insn
	    && targetm.asm_out.unwind_emit)
	  targetm.asm_out.unwind_emit (asm_out_file, insn);

	rtx_call_insn *call_insn = dyn_cast <rtx_call_insn *> (insn);
	if (call_insn != NULL)
	  {
	    rtx x = call_from_call_insn (call_insn);
	    x = XEXP (x, 0);
	    if (x && MEM_P (x) && GET_CODE (XEXP (x, 0)) == SYMBOL_REF)
	      {
		tree t;
		x = XEXP (x, 0);
		t = SYMBOL_REF_DECL (x);
		if (t)
		  assemble_external (t);
	      }
	  }

	/* Output assembler code from the template.  */
	output_asm_insn (templ, recog_data.operand);

	/* Some target machines need to postscan each insn after
	   it is output.  */
	if (targetm.asm_out.final_postscan_insn)
	  targetm.asm_out.final_postscan_insn (file, insn, recog_data.operand,
					       recog_data.n_operands);

	if (!targetm.asm_out.unwind_emit_before_insn
	    && targetm.asm_out.unwind_emit)
	  targetm.asm_out.unwind_emit (asm_out_file, insn);

	/* Let the debug info back-end know about this call.  We do this only
	   after the instruction has been emitted because labels that may be
	   created to reference the call instruction must appear after it.  */
	if (call_insn != NULL && !DECL_IGNORED_P (current_function_decl))
	  debug_hooks->var_location (insn);

	current_output_insn = debug_insn = 0;
      }
    }
  return NEXT_INSN (insn);
}

/* Return whether a source line note needs to be emitted before INSN.
   Sets IS_STMT to TRUE if the line should be marked as a possible
   breakpoint location.  */

static bool
notice_source_line (rtx_insn *insn, bool *is_stmt)
{
  const char *filename;
  int linenum, columnnum;

  if (override_filename)
    {
      filename = override_filename;
      linenum = override_linenum;
      columnnum = override_columnnum;
    }
  else if (INSN_HAS_LOCATION (insn))
    {
      expanded_location xloc = insn_location (insn);
      filename = xloc.file;
      linenum = xloc.line;
      columnnum = xloc.column;
    }
  else
    {
      filename = NULL;
      linenum = 0;
      columnnum = 0;
    }

  if (filename == NULL)
    return false;

  if (force_source_line
      || filename != last_filename
      || last_linenum != linenum
      || (debug_column_info && last_columnnum != columnnum))
    {
      force_source_line = false;
      last_filename = filename;
      last_linenum = linenum;
      last_columnnum = columnnum;
      last_discriminator = discriminator;
      *is_stmt = true;
      high_block_linenum = MAX (last_linenum, high_block_linenum);
      high_function_linenum = MAX (last_linenum, high_function_linenum);
      return true;
    }

  if (SUPPORTS_DISCRIMINATOR && last_discriminator != discriminator)
    {
      /* If the discriminator changed, but the line number did not,
         output the line table entry with is_stmt false so the
         debugger does not treat this as a breakpoint location.  */
      last_discriminator = discriminator;
      *is_stmt = false;
      return true;
    }

  return false;
}

/* For each operand in INSN, simplify (subreg (reg)) so that it refers
   directly to the desired hard register.  */

void
cleanup_subreg_operands (rtx_insn *insn)
{
  int i;
  bool changed = false;
  extract_insn_cached (insn);
  for (i = 0; i < recog_data.n_operands; i++)
    {
      /* The following test cannot use recog_data.operand when testing
	 for a SUBREG: the underlying object might have been changed
	 already if we are inside a match_operator expression that
	 matches the else clause.  Instead we test the underlying
	 expression directly.  */
      if (GET_CODE (*recog_data.operand_loc[i]) == SUBREG)
	{
	  recog_data.operand[i] = alter_subreg (recog_data.operand_loc[i], true);
	  changed = true;
	}
      else if (GET_CODE (recog_data.operand[i]) == PLUS
	       || GET_CODE (recog_data.operand[i]) == MULT
	       || MEM_P (recog_data.operand[i]))
	recog_data.operand[i] = walk_alter_subreg (recog_data.operand_loc[i], &changed);
    }

  for (i = 0; i < recog_data.n_dups; i++)
    {
      if (GET_CODE (*recog_data.dup_loc[i]) == SUBREG)
	{
	  *recog_data.dup_loc[i] = alter_subreg (recog_data.dup_loc[i], true);
	  changed = true;
	}
      else if (GET_CODE (*recog_data.dup_loc[i]) == PLUS
	       || GET_CODE (*recog_data.dup_loc[i]) == MULT
	       || MEM_P (*recog_data.dup_loc[i]))
	*recog_data.dup_loc[i] = walk_alter_subreg (recog_data.dup_loc[i], &changed);
    }
  if (changed)
    df_insn_rescan (insn);
}

/* If X is a SUBREG, try to replace it with a REG or a MEM, based on
   the thing it is a subreg of.  Do it anyway if FINAL_P.  */

rtx
alter_subreg (rtx *xp, bool final_p)
{
  rtx x = *xp;
  rtx y = SUBREG_REG (x);

  /* simplify_subreg does not remove subreg from volatile references.
     We are required to.  */
  if (MEM_P (y))
    {
      int offset = SUBREG_BYTE (x);

      /* For paradoxical subregs on big-endian machines, SUBREG_BYTE
	 contains 0 instead of the proper offset.  See simplify_subreg.  */
      if (paradoxical_subreg_p (x))
        {
          int difference = GET_MODE_SIZE (GET_MODE (y))
			   - GET_MODE_SIZE (GET_MODE (x));
          if (WORDS_BIG_ENDIAN)
            offset += (difference / UNITS_PER_WORD) * UNITS_PER_WORD;
          if (BYTES_BIG_ENDIAN)
            offset += difference % UNITS_PER_WORD;
        }

      if (final_p)
	*xp = adjust_address (y, GET_MODE (x), offset);
      else
	*xp = adjust_address_nv (y, GET_MODE (x), offset);
    }
  else if (REG_P (y) && HARD_REGISTER_P (y))
    {
      rtx new_rtx = simplify_subreg (GET_MODE (x), y, GET_MODE (y),
				     SUBREG_BYTE (x));

      if (new_rtx != 0)
	*xp = new_rtx;
      else if (final_p && REG_P (y))
	{
	  /* Simplify_subreg can't handle some REG cases, but we have to.  */
	  unsigned int regno;
	  HOST_WIDE_INT offset;

	  regno = subreg_regno (x);
	  if (subreg_lowpart_p (x))
	    offset = byte_lowpart_offset (GET_MODE (x), GET_MODE (y));
	  else
	    offset = SUBREG_BYTE (x);
	  *xp = gen_rtx_REG_offset (y, GET_MODE (x), regno, offset);
	}
    }

  return *xp;
}

/* Do alter_subreg on all the SUBREGs contained in X.  */

static rtx
walk_alter_subreg (rtx *xp, bool *changed)
{
  rtx x = *xp;
  switch (GET_CODE (x))
    {
    case PLUS:
    case MULT:
    case AND:
      XEXP (x, 0) = walk_alter_subreg (&XEXP (x, 0), changed);
      XEXP (x, 1) = walk_alter_subreg (&XEXP (x, 1), changed);
      break;

    case MEM:
    case ZERO_EXTEND:
      XEXP (x, 0) = walk_alter_subreg (&XEXP (x, 0), changed);
      break;

    case SUBREG:
      *changed = true;
      return alter_subreg (xp, true);

    default:
      break;
    }

  return *xp;
}

#if HAVE_cc0

/* Given BODY, the body of a jump instruction, alter the jump condition
   as required by the bits that are set in cc_status.flags.
   Not all of the bits there can be handled at this level in all cases.

   The value is normally 0.
   1 means that the condition has become always true.
   -1 means that the condition has become always false.
   2 means that COND has been altered.  */

static int
alter_cond (rtx cond)
{
  int value = 0;

  if (cc_status.flags & CC_REVERSED)
    {
      value = 2;
      PUT_CODE (cond, swap_condition (GET_CODE (cond)));
    }

  if (cc_status.flags & CC_INVERTED)
    {
      value = 2;
      PUT_CODE (cond, reverse_condition (GET_CODE (cond)));
    }

  if (cc_status.flags & CC_NOT_POSITIVE)
    switch (GET_CODE (cond))
      {
      case LE:
      case LEU:
      case GEU:
	/* Jump becomes unconditional.  */
	return 1;

      case GT:
      case GTU:
      case LTU:
	/* Jump becomes no-op.  */
	return -1;

      case GE:
	PUT_CODE (cond, EQ);
	value = 2;
	break;

      case LT:
	PUT_CODE (cond, NE);
	value = 2;
	break;

      default:
	break;
      }

  if (cc_status.flags & CC_NOT_NEGATIVE)
    switch (GET_CODE (cond))
      {
      case GE:
      case GEU:
	/* Jump becomes unconditional.  */
	return 1;

      case LT:
      case LTU:
	/* Jump becomes no-op.  */
	return -1;

      case LE:
      case LEU:
	PUT_CODE (cond, EQ);
	value = 2;
	break;

      case GT:
      case GTU:
	PUT_CODE (cond, NE);
	value = 2;
	break;

      default:
	break;
      }

  if (cc_status.flags & CC_NO_OVERFLOW)
    switch (GET_CODE (cond))
      {
      case GEU:
	/* Jump becomes unconditional.  */
	return 1;

      case LEU:
	PUT_CODE (cond, EQ);
	value = 2;
	break;

      case GTU:
	PUT_CODE (cond, NE);
	value = 2;
	break;

      case LTU:
	/* Jump becomes no-op.  */
	return -1;

      default:
	break;
      }

  if (cc_status.flags & (CC_Z_IN_NOT_N | CC_Z_IN_N))
    switch (GET_CODE (cond))
      {
      default:
	gcc_unreachable ();

      case NE:
	PUT_CODE (cond, cc_status.flags & CC_Z_IN_N ? GE : LT);
	value = 2;
	break;

      case EQ:
	PUT_CODE (cond, cc_status.flags & CC_Z_IN_N ? LT : GE);
	value = 2;
	break;
      }

  if (cc_status.flags & CC_NOT_SIGNED)
    /* The flags are valid if signed condition operators are converted
       to unsigned.  */
    switch (GET_CODE (cond))
      {
      case LE:
	PUT_CODE (cond, LEU);
	value = 2;
	break;

      case LT:
	PUT_CODE (cond, LTU);
	value = 2;
	break;

      case GT:
	PUT_CODE (cond, GTU);
	value = 2;
	break;

      case GE:
	PUT_CODE (cond, GEU);
	value = 2;
	break;

      default:
	break;
      }

  return value;
}
#endif

/* Report inconsistency between the assembler template and the operands.
   In an `asm', it's the user's fault; otherwise, the compiler's fault.  */

void
output_operand_lossage (const char *cmsgid, ...)
{
  char *fmt_string;
  char *new_message;
  const char *pfx_str;
  va_list ap;

  va_start (ap, cmsgid);

  pfx_str = this_is_asm_operands ? _("invalid 'asm': ") : "output_operand: ";
  fmt_string = xasprintf ("%s%s", pfx_str, _(cmsgid));
  new_message = xvasprintf (fmt_string, ap);

  if (this_is_asm_operands)
    error_for_asm (this_is_asm_operands, "%s", new_message);
  else
    internal_error ("%s", new_message);

  free (fmt_string);
  free (new_message);
  va_end (ap);
}

/* Output of assembler code from a template, and its subroutines.  */

/* Annotate the assembly with a comment describing the pattern and
   alternative used.  */

static void
output_asm_name (void)
{
  if (debug_insn)
    {
      int num = INSN_CODE (debug_insn);
      fprintf (asm_out_file, "\t%s %d\t%s",
	       ASM_COMMENT_START, INSN_UID (debug_insn),
	       insn_data[num].name);
      if (insn_data[num].n_alternatives > 1)
	fprintf (asm_out_file, "/%d", which_alternative + 1);

      if (HAVE_ATTR_length)
	fprintf (asm_out_file, "\t[length = %d]",
		 get_attr_length (debug_insn));

      /* Clear this so only the first assembler insn
	 of any rtl insn will get the special comment for -dp.  */
      debug_insn = 0;
    }
}

/* If OP is a REG or MEM and we can find a MEM_EXPR corresponding to it
   or its address, return that expr .  Set *PADDRESSP to 1 if the expr
   corresponds to the address of the object and 0 if to the object.  */

static tree
get_mem_expr_from_op (rtx op, int *paddressp)
{
  tree expr;
  int inner_addressp;

  *paddressp = 0;

  if (REG_P (op))
    return REG_EXPR (op);
  else if (!MEM_P (op))
    return 0;

  if (MEM_EXPR (op) != 0)
    return MEM_EXPR (op);

  /* Otherwise we have an address, so indicate it and look at the address.  */
  *paddressp = 1;
  op = XEXP (op, 0);

  /* First check if we have a decl for the address, then look at the right side
     if it is a PLUS.  Otherwise, strip off arithmetic and keep looking.
     But don't allow the address to itself be indirect.  */
  if ((expr = get_mem_expr_from_op (op, &inner_addressp)) && ! inner_addressp)
    return expr;
  else if (GET_CODE (op) == PLUS
	   && (expr = get_mem_expr_from_op (XEXP (op, 1), &inner_addressp)))
    return expr;

  while (UNARY_P (op)
	 || GET_RTX_CLASS (GET_CODE (op)) == RTX_BIN_ARITH)
    op = XEXP (op, 0);

  expr = get_mem_expr_from_op (op, &inner_addressp);
  return inner_addressp ? 0 : expr;
}

/* Output operand names for assembler instructions.  OPERANDS is the
   operand vector, OPORDER is the order to write the operands, and NOPS
   is the number of operands to write.  */

static void
output_asm_operand_names (rtx *operands, int *oporder, int nops)
{
  int wrote = 0;
  int i;

  for (i = 0; i < nops; i++)
    {
      int addressp;
      rtx op = operands[oporder[i]];
      tree expr = get_mem_expr_from_op (op, &addressp);

      fprintf (asm_out_file, "%c%s",
	       wrote ? ',' : '\t', wrote ? "" : ASM_COMMENT_START);
      wrote = 1;
      if (expr)
	{
	  fprintf (asm_out_file, "%s",
		   addressp ? "*" : "");
	  print_mem_expr (asm_out_file, expr);
	  wrote = 1;
	}
      else if (REG_P (op) && ORIGINAL_REGNO (op)
	       && ORIGINAL_REGNO (op) != REGNO (op))
	fprintf (asm_out_file, " tmp%i", ORIGINAL_REGNO (op));
    }
}

#ifdef ASSEMBLER_DIALECT
/* Helper function to parse assembler dialects in the asm string.
   This is called from output_asm_insn and asm_fprintf.  */
static const char *
do_assembler_dialects (const char *p, int *dialect)
{
  char c = *(p - 1);

  switch (c)
    {
    case '{':
      {
        int i;

        if (*dialect)
          output_operand_lossage ("nested assembly dialect alternatives");
        else
          *dialect = 1;

        /* If we want the first dialect, do nothing.  Otherwise, skip
           DIALECT_NUMBER of strings ending with '|'.  */
        for (i = 0; i < dialect_number; i++)
          {
            while (*p && *p != '}')
	      {
		if (*p == '|')
		  {
		    p++;
		    break;
		  }

		/* Skip over any character after a percent sign.  */
		if (*p == '%')
		  p++;
		if (*p)
		  p++;
	      }

            if (*p == '}')
	      break;
          }

        if (*p == '\0')
          output_operand_lossage ("unterminated assembly dialect alternative");
      }
      break;

    case '|':
      if (*dialect)
        {
          /* Skip to close brace.  */
          do
            {
	      if (*p == '\0')
		{
		  output_operand_lossage ("unterminated assembly dialect alternative");
		  break;
		}

	      /* Skip over any character after a percent sign.  */
	      if (*p == '%' && p[1])
		{
		  p += 2;
		  continue;
		}

	      if (*p++ == '}')
		break;
            }
          while (1);

          *dialect = 0;
        }
      else
        putc (c, asm_out_file);
      break;

    case '}':
      if (! *dialect)
        putc (c, asm_out_file);
      *dialect = 0;
      break;
    default:
      gcc_unreachable ();
    }

  return p;
}
#endif

/* Output text from TEMPLATE to the assembler output file,
   obeying %-directions to substitute operands taken from
   the vector OPERANDS.

   %N (for N a digit) means print operand N in usual manner.
   %lN means require operand N to be a CODE_LABEL or LABEL_REF
      and print the label name with no punctuation.
   %cN means require operand N to be a constant
      and print the constant expression with no punctuation.
   %aN means expect operand N to be a memory address
      (not a memory reference!) and print a reference
      to that address.
   %nN means expect operand N to be a constant
      and print a constant expression for minus the value
      of the operand, with no other punctuation.  */

void
output_asm_insn (const char *templ, rtx *operands)
{
  const char *p;
  int c;
#ifdef ASSEMBLER_DIALECT
  int dialect = 0;
#endif
  int oporder[MAX_RECOG_OPERANDS];
  char opoutput[MAX_RECOG_OPERANDS];
  int ops = 0;

  /* An insn may return a null string template
     in a case where no assembler code is needed.  */
  if (*templ == 0)
    return;

  memset (opoutput, 0, sizeof opoutput);
  p = templ;
  putc ('\t', asm_out_file);

#ifdef ASM_OUTPUT_OPCODE
  ASM_OUTPUT_OPCODE (asm_out_file, p);
#endif

  while ((c = *p++))
    switch (c)
      {
      case '\n':
	if (flag_verbose_asm)
	  output_asm_operand_names (operands, oporder, ops);
	if (flag_print_asm_name)
	  output_asm_name ();

	ops = 0;
	memset (opoutput, 0, sizeof opoutput);

	putc (c, asm_out_file);
#ifdef ASM_OUTPUT_OPCODE
	while ((c = *p) == '\t')
	  {
	    putc (c, asm_out_file);
	    p++;
	  }
	ASM_OUTPUT_OPCODE (asm_out_file, p);
#endif
	break;

#ifdef ASSEMBLER_DIALECT
      case '{':
      case '}':
      case '|':
	p = do_assembler_dialects (p, &dialect);
	break;
#endif

      case '%':
	/* %% outputs a single %.  %{, %} and %| print {, } and | respectively
	   if ASSEMBLER_DIALECT defined and these characters have a special
	   meaning as dialect delimiters.*/
	if (*p == '%'
#ifdef ASSEMBLER_DIALECT
	    || *p == '{' || *p == '}' || *p == '|'
#endif
	    )
	  {
	    putc (*p, asm_out_file);
	    p++;
	  }
	/* %= outputs a number which is unique to each insn in the entire
	   compilation.  This is useful for making local labels that are
	   referred to more than once in a given insn.  */
	else if (*p == '=')
	  {
	    p++;
	    fprintf (asm_out_file, "%d", insn_counter);
	  }
	/* % followed by a letter and some digits
	   outputs an operand in a special way depending on the letter.
	   Letters `acln' are implemented directly.
	   Other letters are passed to `output_operand' so that
	   the TARGET_PRINT_OPERAND hook can define them.  */
	else if (ISALPHA (*p))
	  {
	    int letter = *p++;
	    unsigned long opnum;
	    char *endptr;

	    opnum = strtoul (p, &endptr, 10);

	    if (endptr == p)
	      output_operand_lossage ("operand number missing "
				      "after %%-letter");
	    else if (this_is_asm_operands && opnum >= insn_noperands)
	      output_operand_lossage ("operand number out of range");
	    else if (letter == 'l')
	      output_asm_label (operands[opnum]);
	    else if (letter == 'a')
	      output_address (VOIDmode, operands[opnum]);
	    else if (letter == 'c')
	      {
		if (CONSTANT_ADDRESS_P (operands[opnum]))
		  output_addr_const (asm_out_file, operands[opnum]);
		else
		  output_operand (operands[opnum], 'c');
	      }
	    else if (letter == 'n')
	      {
		if (CONST_INT_P (operands[opnum]))
		  fprintf (asm_out_file, HOST_WIDE_INT_PRINT_DEC,
			   - INTVAL (operands[opnum]));
		else
		  {
		    putc ('-', asm_out_file);
		    output_addr_const (asm_out_file, operands[opnum]);
		  }
	      }
	    else
	      output_operand (operands[opnum], letter);

	    if (!opoutput[opnum])
	      oporder[ops++] = opnum;
	    opoutput[opnum] = 1;

	    p = endptr;
	    c = *p;
	  }
	/* % followed by a digit outputs an operand the default way.  */
	else if (ISDIGIT (*p))
	  {
	    unsigned long opnum;
	    char *endptr;

	    opnum = strtoul (p, &endptr, 10);
	    if (this_is_asm_operands && opnum >= insn_noperands)
	      output_operand_lossage ("operand number out of range");
	    else
	      output_operand (operands[opnum], 0);

	    if (!opoutput[opnum])
	      oporder[ops++] = opnum;
	    opoutput[opnum] = 1;

	    p = endptr;
	    c = *p;
	  }
	/* % followed by punctuation: output something for that
	   punctuation character alone, with no operand.  The
	   TARGET_PRINT_OPERAND hook decides what is actually done.  */
	else if (targetm.asm_out.print_operand_punct_valid_p ((unsigned char) *p))
	  output_operand (NULL_RTX, *p++);
	else
	  output_operand_lossage ("invalid %%-code");
	break;

      default:
	putc (c, asm_out_file);
      }

  /* Write out the variable names for operands, if we know them.  */
  if (flag_verbose_asm)
    output_asm_operand_names (operands, oporder, ops);
  if (flag_print_asm_name)
    output_asm_name ();

  putc ('\n', asm_out_file);
}

/* Output a LABEL_REF, or a bare CODE_LABEL, as an assembler symbol.  */

void
output_asm_label (rtx x)
{
  char buf[256];

  if (GET_CODE (x) == LABEL_REF)
    x = label_ref_label (x);
  if (LABEL_P (x)
      || (NOTE_P (x)
	  && NOTE_KIND (x) == NOTE_INSN_DELETED_LABEL))
    ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (x));
  else
    output_operand_lossage ("'%%l' operand isn't a label");

  assemble_name (asm_out_file, buf);
}

/* Marks SYMBOL_REFs in x as referenced through use of assemble_external.  */

void
mark_symbol_refs_as_used (rtx x)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, x, ALL)
    {
      const_rtx x = *iter;
      if (GET_CODE (x) == SYMBOL_REF)
	if (tree t = SYMBOL_REF_DECL (x))
	  assemble_external (t);
    }
}

/* Print operand X using machine-dependent assembler syntax.
   CODE is a non-digit that preceded the operand-number in the % spec,
   such as 'z' if the spec was `%z3'.  CODE is 0 if there was no char
   between the % and the digits.
   When CODE is a non-letter, X is 0.

   The meanings of the letters are machine-dependent and controlled
   by TARGET_PRINT_OPERAND.  */

void
output_operand (rtx x, int code ATTRIBUTE_UNUSED)
{
  if (x && GET_CODE (x) == SUBREG)
    x = alter_subreg (&x, true);

  /* X must not be a pseudo reg.  */
  if (!targetm.no_register_allocation)
    gcc_assert (!x || !REG_P (x) || REGNO (x) < FIRST_PSEUDO_REGISTER);

  targetm.asm_out.print_operand (asm_out_file, x, code);

  if (x == NULL_RTX)
    return;

  mark_symbol_refs_as_used (x);
}

/* Print a memory reference operand for address X using
   machine-dependent assembler syntax.  */

void
output_address (machine_mode mode, rtx x)
{
  bool changed = false;
  walk_alter_subreg (&x, &changed);
  targetm.asm_out.print_operand_address (asm_out_file, mode, x);
}

/* Print an integer constant expression in assembler syntax.
   Addition and subtraction are the only arithmetic
   that may appear in these expressions.  */

void
output_addr_const (FILE *file, rtx x)
{
  char buf[256];

 restart:
  switch (GET_CODE (x))
    {
    case PC:
      putc ('.', file);
      break;

    case SYMBOL_REF:
      if (SYMBOL_REF_DECL (x))
	assemble_external (SYMBOL_REF_DECL (x));
#ifdef ASM_OUTPUT_SYMBOL_REF
      ASM_OUTPUT_SYMBOL_REF (file, x);
#else
      assemble_name (file, XSTR (x, 0));
#endif
      break;

    case LABEL_REF:
      x = label_ref_label (x);
      /* Fall through.  */
    case CODE_LABEL:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (x));
#ifdef ASM_OUTPUT_LABEL_REF
      ASM_OUTPUT_LABEL_REF (file, buf);
#else
      assemble_name (file, buf);
#endif
      break;

    case CONST_INT:
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
      break;

    case CONST:
      /* This used to output parentheses around the expression,
	 but that does not work on the 386 (either ATT or BSD assembler).  */
      output_addr_const (file, XEXP (x, 0));
      break;

    case CONST_WIDE_INT:
      /* We do not know the mode here so we have to use a round about
	 way to build a wide-int to get it printed properly.  */
      {
	wide_int w = wide_int::from_array (&CONST_WIDE_INT_ELT (x, 0),
					   CONST_WIDE_INT_NUNITS (x),
					   CONST_WIDE_INT_NUNITS (x)
					   * HOST_BITS_PER_WIDE_INT,
					   false);
	print_decs (w, file);
      }
      break;

    case CONST_DOUBLE:
      if (CONST_DOUBLE_AS_INT_P (x))
	{
	  /* We can use %d if the number is one word and positive.  */
	  if (CONST_DOUBLE_HIGH (x))
	    fprintf (file, HOST_WIDE_INT_PRINT_DOUBLE_HEX,
		     (unsigned HOST_WIDE_INT) CONST_DOUBLE_HIGH (x),
		     (unsigned HOST_WIDE_INT) CONST_DOUBLE_LOW (x));
	  else if (CONST_DOUBLE_LOW (x) < 0)
	    fprintf (file, HOST_WIDE_INT_PRINT_HEX,
		     (unsigned HOST_WIDE_INT) CONST_DOUBLE_LOW (x));
	  else
	    fprintf (file, HOST_WIDE_INT_PRINT_DEC, CONST_DOUBLE_LOW (x));
	}
      else
	/* We can't handle floating point constants;
	   PRINT_OPERAND must handle them.  */
	output_operand_lossage ("floating constant misused");
      break;

    case CONST_FIXED:
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, CONST_FIXED_VALUE_LOW (x));
      break;

    case PLUS:
      /* Some assemblers need integer constants to appear last (eg masm).  */
      if (CONST_INT_P (XEXP (x, 0)))
	{
	  output_addr_const (file, XEXP (x, 1));
	  if (INTVAL (XEXP (x, 0)) >= 0)
	    fprintf (file, "+");
	  output_addr_const (file, XEXP (x, 0));
	}
      else
	{
	  output_addr_const (file, XEXP (x, 0));
	  if (!CONST_INT_P (XEXP (x, 1))
	      || INTVAL (XEXP (x, 1)) >= 0)
	    fprintf (file, "+");
	  output_addr_const (file, XEXP (x, 1));
	}
      break;

    case MINUS:
      /* Avoid outputting things like x-x or x+5-x,
	 since some assemblers can't handle that.  */
      x = simplify_subtraction (x);
      if (GET_CODE (x) != MINUS)
	goto restart;

      output_addr_const (file, XEXP (x, 0));
      fprintf (file, "-");
      if ((CONST_INT_P (XEXP (x, 1)) && INTVAL (XEXP (x, 1)) >= 0)
	  || GET_CODE (XEXP (x, 1)) == PC
	  || GET_CODE (XEXP (x, 1)) == SYMBOL_REF)
	output_addr_const (file, XEXP (x, 1));
      else
	{
	  fputs (targetm.asm_out.open_paren, file);
	  output_addr_const (file, XEXP (x, 1));
	  fputs (targetm.asm_out.close_paren, file);
	}
      break;

    case ZERO_EXTEND:
    case SIGN_EXTEND:
    case SUBREG:
    case TRUNCATE:
      output_addr_const (file, XEXP (x, 0));
      break;

    default:
      if (targetm.asm_out.output_addr_const_extra (file, x))
	break;

      output_operand_lossage ("invalid expression as operand");
    }
}

/* Output a quoted string.  */

void
output_quoted_string (FILE *asm_file, const char *string)
{
#ifdef OUTPUT_QUOTED_STRING
  OUTPUT_QUOTED_STRING (asm_file, string);
#else
  char c;

  putc ('\"', asm_file);
  while ((c = *string++) != 0)
    {
      if (ISPRINT (c))
	{
	  if (c == '\"' || c == '\\')
	    putc ('\\', asm_file);
	  putc (c, asm_file);
	}
      else
	fprintf (asm_file, "\\%03o", (unsigned char) c);
    }
  putc ('\"', asm_file);
#endif
}

/* Write a HOST_WIDE_INT number in hex form 0x1234, fast. */

void
fprint_whex (FILE *f, unsigned HOST_WIDE_INT value)
{
  char buf[2 + CHAR_BIT * sizeof (value) / 4];
  if (value == 0)
    putc ('0', f);
  else
    {
      char *p = buf + sizeof (buf);
      do
        *--p = "0123456789abcdef"[value % 16];
      while ((value /= 16) != 0);
      *--p = 'x';
      *--p = '0';
      fwrite (p, 1, buf + sizeof (buf) - p, f);
    }
}

/* Internal function that prints an unsigned long in decimal in reverse.
   The output string IS NOT null-terminated. */

static int
sprint_ul_rev (char *s, unsigned long value)
{
  int i = 0;
  do
    {
      s[i] = "0123456789"[value % 10];
      value /= 10;
      i++;
      /* alternate version, without modulo */
      /* oldval = value; */
      /* value /= 10; */
      /* s[i] = "0123456789" [oldval - 10*value]; */
      /* i++ */
    }
  while (value != 0);
  return i;
}

/* Write an unsigned long as decimal to a file, fast. */

void
fprint_ul (FILE *f, unsigned long value)
{
  /* python says: len(str(2**64)) == 20 */
  char s[20];
  int i;

  i = sprint_ul_rev (s, value);

  /* It's probably too small to bother with string reversal and fputs. */
  do
    {
      i--;
      putc (s[i], f);
    }
  while (i != 0);
}

/* Write an unsigned long as decimal to a string, fast.
   s must be wide enough to not overflow, at least 21 chars.
   Returns the length of the string (without terminating '\0'). */

int
sprint_ul (char *s, unsigned long value)
{
  int len = sprint_ul_rev (s, value);
  s[len] = '\0';

  std::reverse (s, s + len);
  return len;
}

/* A poor man's fprintf, with the added features of %I, %R, %L, and %U.
   %R prints the value of REGISTER_PREFIX.
   %L prints the value of LOCAL_LABEL_PREFIX.
   %U prints the value of USER_LABEL_PREFIX.
   %I prints the value of IMMEDIATE_PREFIX.
   %O runs ASM_OUTPUT_OPCODE to transform what follows in the string.
   Also supported are %d, %i, %u, %x, %X, %o, %c, %s and %%.

   We handle alternate assembler dialects here, just like output_asm_insn.  */

void
asm_fprintf (FILE *file, const char *p, ...)
{
  char buf[10];
  char *q, c;
#ifdef ASSEMBLER_DIALECT
  int dialect = 0;
#endif
  va_list argptr;

  va_start (argptr, p);

  buf[0] = '%';

  while ((c = *p++))
    switch (c)
      {
#ifdef ASSEMBLER_DIALECT
      case '{':
      case '}':
      case '|':
	p = do_assembler_dialects (p, &dialect);
	break;
#endif

      case '%':
	c = *p++;
	q = &buf[1];
	while (strchr ("-+ #0", c))
	  {
	    *q++ = c;
	    c = *p++;
	  }
	while (ISDIGIT (c) || c == '.')
	  {
	    *q++ = c;
	    c = *p++;
	  }
	switch (c)
	  {
	  case '%':
	    putc ('%', file);
	    break;

	  case 'd':  case 'i':  case 'u':
	  case 'x':  case 'X':  case 'o':
	  case 'c':
	    *q++ = c;
	    *q = 0;
	    fprintf (file, buf, va_arg (argptr, int));
	    break;

	  case 'w':
	    /* This is a prefix to the 'd', 'i', 'u', 'x', 'X', and
	       'o' cases, but we do not check for those cases.  It
	       means that the value is a HOST_WIDE_INT, which may be
	       either `long' or `long long'.  */
	    memcpy (q, HOST_WIDE_INT_PRINT, strlen (HOST_WIDE_INT_PRINT));
	    q += strlen (HOST_WIDE_INT_PRINT);
	    *q++ = *p++;
	    *q = 0;
	    fprintf (file, buf, va_arg (argptr, HOST_WIDE_INT));
	    break;

	  case 'l':
	    *q++ = c;
#ifdef HAVE_LONG_LONG
	    if (*p == 'l')
	      {
		*q++ = *p++;
		*q++ = *p++;
		*q = 0;
		fprintf (file, buf, va_arg (argptr, long long));
	      }
	    else
#endif
	      {
		*q++ = *p++;
		*q = 0;
		fprintf (file, buf, va_arg (argptr, long));
	      }

	    break;

	  case 's':
	    *q++ = c;
	    *q = 0;
	    fprintf (file, buf, va_arg (argptr, char *));
	    break;

	  case 'O':
#ifdef ASM_OUTPUT_OPCODE
	    ASM_OUTPUT_OPCODE (asm_out_file, p);
#endif
	    break;

	  case 'R':
#ifdef REGISTER_PREFIX
	    fprintf (file, "%s", REGISTER_PREFIX);
#endif
	    break;

	  case 'I':
#ifdef IMMEDIATE_PREFIX
	    fprintf (file, "%s", IMMEDIATE_PREFIX);
#endif
	    break;

	  case 'L':
#ifdef LOCAL_LABEL_PREFIX
	    fprintf (file, "%s", LOCAL_LABEL_PREFIX);
#endif
	    break;

	  case 'U':
	    fputs (user_label_prefix, file);
	    break;

#ifdef ASM_FPRINTF_EXTENSIONS
	    /* Uppercase letters are reserved for general use by asm_fprintf
	       and so are not available to target specific code.  In order to
	       prevent the ASM_FPRINTF_EXTENSIONS macro from using them then,
	       they are defined here.  As they get turned into real extensions
	       to asm_fprintf they should be removed from this list.  */
	  case 'A': case 'B': case 'C': case 'D': case 'E':
	  case 'F': case 'G': case 'H': case 'J': case 'K':
	  case 'M': case 'N': case 'P': case 'Q': case 'S':
	  case 'T': case 'V': case 'W': case 'Y': case 'Z':
	    break;

	  ASM_FPRINTF_EXTENSIONS (file, argptr, p)
#endif
	  default:
	    gcc_unreachable ();
	  }
	break;

      default:
	putc (c, file);
      }
  va_end (argptr);
}

/* Return nonzero if this function has no function calls.  */

int
leaf_function_p (void)
{
  rtx_insn *insn;

  /* Ensure we walk the entire function body.  */
  gcc_assert (!in_sequence_p ());

  /* Some back-ends (e.g. s390) want leaf functions to stay leaf
     functions even if they call mcount.  */
  if (crtl->profile && !targetm.keep_leaf_when_profiled ())
    return 0;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (CALL_P (insn)
	  && ! SIBLING_CALL_P (insn))
	return 0;
      if (NONJUMP_INSN_P (insn)
	  && GET_CODE (PATTERN (insn)) == SEQUENCE
	  && CALL_P (XVECEXP (PATTERN (insn), 0, 0))
	  && ! SIBLING_CALL_P (XVECEXP (PATTERN (insn), 0, 0)))
	return 0;
    }

  return 1;
}

/* Return 1 if branch is a forward branch.
   Uses insn_shuid array, so it works only in the final pass.  May be used by
   output templates to customary add branch prediction hints.
 */
int
final_forward_branch_p (rtx_insn *insn)
{
  int insn_id, label_id;

  gcc_assert (uid_shuid);
  insn_id = INSN_SHUID (insn);
  label_id = INSN_SHUID (JUMP_LABEL (insn));
  /* We've hit some insns that does not have id information available.  */
  gcc_assert (insn_id && label_id);
  return insn_id < label_id;
}

/* On some machines, a function with no call insns
   can run faster if it doesn't create its own register window.
   When output, the leaf function should use only the "output"
   registers.  Ordinarily, the function would be compiled to use
   the "input" registers to find its arguments; it is a candidate
   for leaf treatment if it uses only the "input" registers.
   Leaf function treatment means renumbering so the function
   uses the "output" registers instead.  */

#ifdef LEAF_REGISTERS

/* Return 1 if this function uses only the registers that can be
   safely renumbered.  */

int
only_leaf_regs_used (void)
{
  int i;
  const char *const permitted_reg_in_leaf_functions = LEAF_REGISTERS;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if ((df_regs_ever_live_p (i) || global_regs[i])
	&& ! permitted_reg_in_leaf_functions[i])
      return 0;

  if (crtl->uses_pic_offset_table
      && pic_offset_table_rtx != 0
      && REG_P (pic_offset_table_rtx)
      && ! permitted_reg_in_leaf_functions[REGNO (pic_offset_table_rtx)])
    return 0;

  return 1;
}

/* Scan all instructions and renumber all registers into those
   available in leaf functions.  */

static void
leaf_renumber_regs (rtx_insn *first)
{
  rtx_insn *insn;

  /* Renumber only the actual patterns.
     The reg-notes can contain frame pointer refs,
     and renumbering them could crash, and should not be needed.  */
  for (insn = first; insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      leaf_renumber_regs_insn (PATTERN (insn));
}

/* Scan IN_RTX and its subexpressions, and renumber all regs into those
   available in leaf functions.  */

void
leaf_renumber_regs_insn (rtx in_rtx)
{
  int i, j;
  const char *format_ptr;

  if (in_rtx == 0)
    return;

  /* Renumber all input-registers into output-registers.
     renumbered_regs would be 1 for an output-register;
     they  */

  if (REG_P (in_rtx))
    {
      int newreg;

      /* Don't renumber the same reg twice.  */
      if (in_rtx->used)
	return;

      newreg = REGNO (in_rtx);
      /* Don't try to renumber pseudo regs.  It is possible for a pseudo reg
	 to reach here as part of a REG_NOTE.  */
      if (newreg >= FIRST_PSEUDO_REGISTER)
	{
	  in_rtx->used = 1;
	  return;
	}
      newreg = LEAF_REG_REMAP (newreg);
      gcc_assert (newreg >= 0);
      df_set_regs_ever_live (REGNO (in_rtx), false);
      df_set_regs_ever_live (newreg, true);
      SET_REGNO (in_rtx, newreg);
      in_rtx->used = 1;
      return;
    }

  if (INSN_P (in_rtx))
    {
      /* Inside a SEQUENCE, we find insns.
	 Renumber just the patterns of these insns,
	 just as we do for the top-level insns.  */
      leaf_renumber_regs_insn (PATTERN (in_rtx));
      return;
    }

  format_ptr = GET_RTX_FORMAT (GET_CODE (in_rtx));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (in_rtx)); i++)
    switch (*format_ptr++)
      {
      case 'e':
	leaf_renumber_regs_insn (XEXP (in_rtx, i));
	break;

      case 'E':
	if (NULL != XVEC (in_rtx, i))
	  {
	    for (j = 0; j < XVECLEN (in_rtx, i); j++)
	      leaf_renumber_regs_insn (XVECEXP (in_rtx, i, j));
	  }
	break;

      case 'S':
      case 's':
      case '0':
      case 'i':
      case 'w':
      case 'n':
      case 'u':
	break;

      default:
	gcc_unreachable ();
      }
}
#endif

/* Turn the RTL into assembly.  */
static unsigned int
rest_of_handle_final (void)
{
  const char *fnname = get_fnname_from_decl (current_function_decl);

  assemble_start_function (current_function_decl, fnname);
  final_start_function (get_insns (), asm_out_file, optimize);
  final (get_insns (), asm_out_file, optimize);
  if (flag_ipa_ra
      && !lookup_attribute ("noipa", DECL_ATTRIBUTES (current_function_decl)))
    collect_fn_hard_reg_usage ();
  final_end_function ();

  /* The IA-64 ".handlerdata" directive must be issued before the ".endp"
     directive that closes the procedure descriptor.  Similarly, for x64 SEH.
     Otherwise it's not strictly necessary, but it doesn't hurt either.  */
  output_function_exception_table (fnname);

  assemble_end_function (current_function_decl, fnname);

  /* Free up reg info memory.  */
  free_reg_info ();

  if (! quiet_flag)
    fflush (asm_out_file);

  /* Write DBX symbols if requested.  */

  /* Note that for those inline functions where we don't initially
     know for certain that we will be generating an out-of-line copy,
     the first invocation of this routine (rest_of_compilation) will
     skip over this code by doing a `goto exit_rest_of_compilation;'.
     Later on, wrapup_global_declarations will (indirectly) call
     rest_of_compilation again for those inline functions that need
     to have out-of-line copies generated.  During that call, we
     *will* be routed past here.  */

  timevar_push (TV_SYMOUT);
  if (!DECL_IGNORED_P (current_function_decl))
    debug_hooks->function_decl (current_function_decl);
  timevar_pop (TV_SYMOUT);

  /* Release the blocks that are linked to DECL_INITIAL() to free the memory.  */
  DECL_INITIAL (current_function_decl) = error_mark_node;

  if (DECL_STATIC_CONSTRUCTOR (current_function_decl)
      && targetm.have_ctors_dtors)
    targetm.asm_out.constructor (XEXP (DECL_RTL (current_function_decl), 0),
				 decl_init_priority_lookup
				   (current_function_decl));
  if (DECL_STATIC_DESTRUCTOR (current_function_decl)
      && targetm.have_ctors_dtors)
    targetm.asm_out.destructor (XEXP (DECL_RTL (current_function_decl), 0),
				decl_fini_priority_lookup
				  (current_function_decl));
  return 0;
}

namespace {

const pass_data pass_data_final =
{
  RTL_PASS, /* type */
  "final", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_FINAL, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_final : public rtl_opt_pass
{
public:
  pass_final (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_final, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *) { return rest_of_handle_final (); }

}; // class pass_final

} // anon namespace

rtl_opt_pass *
make_pass_final (gcc::context *ctxt)
{
  return new pass_final (ctxt);
}


static unsigned int
rest_of_handle_shorten_branches (void)
{
  /* Shorten branches.  */
  shorten_branches (get_insns ());
  return 0;
}

namespace {

const pass_data pass_data_shorten_branches =
{
  RTL_PASS, /* type */
  "shorten", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_SHORTEN_BRANCH, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_shorten_branches : public rtl_opt_pass
{
public:
  pass_shorten_branches (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_shorten_branches, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *)
    {
      return rest_of_handle_shorten_branches ();
    }

}; // class pass_shorten_branches

} // anon namespace

rtl_opt_pass *
make_pass_shorten_branches (gcc::context *ctxt)
{
  return new pass_shorten_branches (ctxt);
}


static unsigned int
rest_of_clean_state (void)
{
  rtx_insn *insn, *next;
  FILE *final_output = NULL;
  int save_unnumbered = flag_dump_unnumbered;
  int save_noaddr = flag_dump_noaddr;

  if (flag_dump_final_insns)
    {
      final_output = fopen (flag_dump_final_insns, "a");
      if (!final_output)
	{
	  error ("could not open final insn dump file %qs: %m",
		 flag_dump_final_insns);
	  flag_dump_final_insns = NULL;
	}
      else
	{
	  flag_dump_noaddr = flag_dump_unnumbered = 1;
	  if (flag_compare_debug_opt || flag_compare_debug)
	    dump_flags |= TDF_NOUID;
	  dump_function_header (final_output, current_function_decl,
				dump_flags);
	  final_insns_dump_p = true;

	  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	    if (LABEL_P (insn))
	      INSN_UID (insn) = CODE_LABEL_NUMBER (insn);
	    else
	      {
		if (NOTE_P (insn))
		  set_block_for_insn (insn, NULL);
		INSN_UID (insn) = 0;
	      }
	}
    }

  /* It is very important to decompose the RTL instruction chain here:
     debug information keeps pointing into CODE_LABEL insns inside the function
     body.  If these remain pointing to the other insns, we end up preserving
     whole RTL chain and attached detailed debug info in memory.  */
  for (insn = get_insns (); insn; insn = next)
    {
      next = NEXT_INSN (insn);
      SET_NEXT_INSN (insn) = NULL;
      SET_PREV_INSN (insn) = NULL;

      if (final_output
	  && (!NOTE_P (insn) ||
	      (NOTE_KIND (insn) != NOTE_INSN_VAR_LOCATION
	       && NOTE_KIND (insn) != NOTE_INSN_CALL_ARG_LOCATION
	       && NOTE_KIND (insn) != NOTE_INSN_BLOCK_BEG
	       && NOTE_KIND (insn) != NOTE_INSN_BLOCK_END
	       && NOTE_KIND (insn) != NOTE_INSN_DELETED_DEBUG_LABEL)))
	print_rtl_single (final_output, insn);
    }

  if (final_output)
    {
      flag_dump_noaddr = save_noaddr;
      flag_dump_unnumbered = save_unnumbered;
      final_insns_dump_p = false;

      if (fclose (final_output))
	{
	  error ("could not close final insn dump file %qs: %m",
		 flag_dump_final_insns);
	  flag_dump_final_insns = NULL;
	}
    }

  /* In case the function was not output,
     don't leave any temporary anonymous types
     queued up for sdb output.  */
  if (SDB_DEBUGGING_INFO && write_symbols == SDB_DEBUG)
    sdbout_types (NULL_TREE);

  flag_rerun_cse_after_global_opts = 0;
  reload_completed = 0;
  epilogue_completed = 0;
#ifdef STACK_REGS
  regstack_completed = 0;
#endif

  /* Clear out the insn_length contents now that they are no
     longer valid.  */
  init_insn_lengths ();

  /* Show no temporary slots allocated.  */
  init_temp_slots ();

  free_bb_for_insn ();

  if (cfun->gimple_df)
    delete_tree_ssa (cfun);

  /* We can reduce stack alignment on call site only when we are sure that
     the function body just produced will be actually used in the final
     executable.  */
  if (decl_binds_to_current_def_p (current_function_decl))
    {
      unsigned int pref = crtl->preferred_stack_boundary;
      if (crtl->stack_alignment_needed > crtl->preferred_stack_boundary)
        pref = crtl->stack_alignment_needed;
      cgraph_node::rtl_info (current_function_decl)
	->preferred_incoming_stack_boundary = pref;
    }

  /* Make sure volatile mem refs aren't considered valid operands for
     arithmetic insns.  We must call this here if this is a nested inline
     function, since the above code leaves us in the init_recog state,
     and the function context push/pop code does not save/restore volatile_ok.

     ??? Maybe it isn't necessary for expand_start_function to call this
     anymore if we do it here?  */

  init_recog_no_volatile ();

  /* We're done with this function.  Free up memory if we can.  */
  free_after_parsing (cfun);
  free_after_compilation (cfun);
  return 0;
}

namespace {

const pass_data pass_data_clean_state =
{
  RTL_PASS, /* type */
  "*clean_state", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_FINAL, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  PROP_rtl, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_clean_state : public rtl_opt_pass
{
public:
  pass_clean_state (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_clean_state, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *)
    {
      return rest_of_clean_state ();
    }

}; // class pass_clean_state

} // anon namespace

rtl_opt_pass *
make_pass_clean_state (gcc::context *ctxt)
{
  return new pass_clean_state (ctxt);
}

/* Return true if INSN is a call to the current function.  */

static bool
self_recursive_call_p (rtx_insn *insn)
{
  tree fndecl = get_call_fndecl (insn);
  return (fndecl == current_function_decl
	  && decl_binds_to_current_def_p (fndecl));
}

/* Collect hard register usage for the current function.  */

static void
collect_fn_hard_reg_usage (void)
{
  rtx_insn *insn;
#ifdef STACK_REGS
  int i;
#endif
  struct cgraph_rtl_info *node;
  HARD_REG_SET function_used_regs;

  /* ??? To be removed when all the ports have been fixed.  */
  if (!targetm.call_fusage_contains_non_callee_clobbers)
    return;

  CLEAR_HARD_REG_SET (function_used_regs);

  for (insn = get_insns (); insn != NULL_RTX; insn = next_insn (insn))
    {
      HARD_REG_SET insn_used_regs;

      if (!NONDEBUG_INSN_P (insn))
	continue;

      if (CALL_P (insn)
	  && !self_recursive_call_p (insn))
	{
	  if (!get_call_reg_set_usage (insn, &insn_used_regs,
				       call_used_reg_set))
	    return;

	  IOR_HARD_REG_SET (function_used_regs, insn_used_regs);
	}

      find_all_hard_reg_sets (insn, &insn_used_regs, false);
      IOR_HARD_REG_SET (function_used_regs, insn_used_regs);
    }

  /* Be conservative - mark fixed and global registers as used.  */
  IOR_HARD_REG_SET (function_used_regs, fixed_reg_set);

#ifdef STACK_REGS
  /* Handle STACK_REGS conservatively, since the df-framework does not
     provide accurate information for them.  */

  for (i = FIRST_STACK_REG; i <= LAST_STACK_REG; i++)
    SET_HARD_REG_BIT (function_used_regs, i);
#endif

  /* The information we have gathered is only interesting if it exposes a
     register from the call_used_regs that is not used in this function.  */
  if (hard_reg_set_subset_p (call_used_reg_set, function_used_regs))
    return;

  node = cgraph_node::rtl_info (current_function_decl);
  gcc_assert (node != NULL);

  COPY_HARD_REG_SET (node->function_used_regs, function_used_regs);
  node->function_used_regs_valid = 1;
}

/* Get the declaration of the function called by INSN.  */

static tree
get_call_fndecl (rtx_insn *insn)
{
  rtx note, datum;

  note = find_reg_note (insn, REG_CALL_DECL, NULL_RTX);
  if (note == NULL_RTX)
    return NULL_TREE;

  datum = XEXP (note, 0);
  if (datum != NULL_RTX)
    return SYMBOL_REF_DECL (datum);

  return NULL_TREE;
}

/* Return the cgraph_rtl_info of the function called by INSN.  Returns NULL for
   call targets that can be overwritten.  */

static struct cgraph_rtl_info *
get_call_cgraph_rtl_info (rtx_insn *insn)
{
  tree fndecl;

  if (insn == NULL_RTX)
    return NULL;

  fndecl = get_call_fndecl (insn);
  if (fndecl == NULL_TREE
      || !decl_binds_to_current_def_p (fndecl))
    return NULL;

  return cgraph_node::rtl_info (fndecl);
}

/* Find hard registers used by function call instruction INSN, and return them
   in REG_SET.  Return DEFAULT_SET in REG_SET if not found.  */

bool
get_call_reg_set_usage (rtx_insn *insn, HARD_REG_SET *reg_set,
			HARD_REG_SET default_set)
{
  if (flag_ipa_ra)
    {
      struct cgraph_rtl_info *node = get_call_cgraph_rtl_info (insn);
      if (node != NULL
	  && node->function_used_regs_valid)
	{
	  COPY_HARD_REG_SET (*reg_set, node->function_used_regs);
	  AND_HARD_REG_SET (*reg_set, default_set);
	  return true;
	}
    }

  COPY_HARD_REG_SET (*reg_set, default_set);
  return false;
}
