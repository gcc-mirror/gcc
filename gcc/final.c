/* Convert RTL to assembler code and output it, for GNU compiler.
   Copyright (C) 1987, 88, 89, 92-6, 1997 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


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
   directly as assembler code by the macros FUNCTION_PROLOGUE and
   FUNCTION_EPILOGUE.  Those instructions never exist as rtl.  */

#include "config.h"
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <stdio.h>
#include <ctype.h>

#include "tree.h"
#include "rtl.h"
#include "regs.h"
#include "insn-config.h"
#include "insn-flags.h"
#include "insn-attr.h"
#include "insn-codes.h"
#include "recog.h"
#include "conditions.h"
#include "flags.h"
#include "real.h"
#include "hard-reg-set.h"
#include "defaults.h"
#include "output.h"
#include "except.h"

/* Get N_SLINE and N_SOL from stab.h if we can expect the file to exist.  */
#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
#if defined (USG) || defined (NO_STAB_H)
#include "gstab.h"  /* If doing DBX on sysV, use our own stab.h.  */
#else
#include <stab.h>  /* On BSD, use the system's stab.h.  */
#endif /* not USG */
#endif /* DBX_DEBUGGING_INFO || XCOFF_DEBUGGING_INFO */

#ifdef XCOFF_DEBUGGING_INFO
#include "xcoffout.h"
#endif

/* .stabd code for line number.  */
#ifndef N_SLINE
#define	N_SLINE	0x44
#endif

/* .stabs code for included file name.  */
#ifndef N_SOL
#define	N_SOL 0x84
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

/* If we aren't using cc0, CC_STATUS_INIT shouldn't exist.  So define a
   null default for it to save conditionalization later.  */
#ifndef CC_STATUS_INIT
#define CC_STATUS_INIT
#endif

/* How to start an assembler comment.  */
#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START ";#"
#endif

/* Is the given character a logical line separator for the assembler?  */
#ifndef IS_ASM_LOGICAL_LINE_SEPARATOR
#define IS_ASM_LOGICAL_LINE_SEPARATOR(C) ((C) == ';')
#endif

/* Nonzero means this function is a leaf function, with no function calls. 
   This variable exists to be examined in FUNCTION_PROLOGUE
   and FUNCTION_EPILOGUE.  Always zero, unless set by some action.  */
int leaf_function;

/* Last insn processed by final_scan_insn.  */
static rtx debug_insn = 0;

/* Line number of last NOTE.  */
static int last_linenum;

/* Highest line number in current block.  */
static int high_block_linenum;

/* Likewise for function.  */
static int high_function_linenum;

/* Filename of last NOTE.  */
static char *last_filename;

/* Number of basic blocks seen so far;
   used if profile_block_flag is set.  */
static int count_basic_blocks;

/* Number of instrumented arcs when profile_arc_flag is set.  */
extern int count_instrumented_arcs;

/* Nonzero while outputting an `asm' with operands.
   This means that inconsistencies are the user's fault, so don't abort.
   The precise value is the insn being output, to pass to error_for_asm.  */
static rtx this_is_asm_operands;

/* Number of operands of this insn, for an `asm' with operands.  */
static int insn_noperands;

/* Compare optimization flag.  */

static rtx last_ignored_compare = 0;

/* Flag indicating this insn is the start of a new basic block.  */

static int new_block = 1;

/* All the symbol-blocks (levels of scoping) in the compilation
   are assigned sequence numbers in order of appearance of the
   beginnings of the symbol-blocks.  Both final and dbxout do this,
   and assume that they will both give the same number to each block.
   Final uses these sequence numbers to generate assembler label names
   LBBnnn and LBEnnn for the beginning and end of the symbol-block.
   Dbxout uses the sequence numbers to generate references to the same labels
   from the dbx debugging information.

   Sdb records this level at the beginning of each function,
   in order to find the current level when recursing down declarations.
   It outputs the block beginning and endings
   at the point in the asm file where the blocks would begin and end.  */

int next_block_index;

/* Assign a unique number to each insn that is output.
   This can be used to generate unique local labels.  */

static int insn_counter = 0;

#ifdef HAVE_cc0
/* This variable contains machine-dependent flags (defined in tm.h)
   set and examined by output routines
   that describe how to interpret the condition codes properly.  */

CC_STATUS cc_status;

/* During output of an insn, this contains a copy of cc_status
   from before the insn.  */

CC_STATUS cc_prev_status;
#endif

/* Indexed by hardware reg number, is 1 if that register is ever
   used in the current function.

   In life_analysis, or in stupid_life_analysis, this is set
   up to record the hard regs used explicitly.  Reload adds
   in the hard regs used for holding pseudo regs.  Final uses
   it to generate the code in the function prologue and epilogue
   to save and restore registers as needed.  */

char regs_ever_live[FIRST_PSEUDO_REGISTER];

/* Nonzero means current function must be given a frame pointer.
   Set in stmt.c if anything is allocated on the stack there.
   Set in reload1.c if anything is allocated on the stack there.  */

int frame_pointer_needed;

/* Assign unique numbers to labels generated for profiling.  */

int profile_label_no;

/* Length so far allocated in PENDING_BLOCKS.  */

static int max_block_depth;

/* Stack of sequence numbers of symbol-blocks of which we have seen the
   beginning but not yet the end.  Sequence numbers are assigned at
   the beginning; this stack allows us to find the sequence number
   of a block that is ending.  */

static int *pending_blocks;

/* Number of elements currently in use in PENDING_BLOCKS.  */

static int block_depth;

/* Nonzero if have enabled APP processing of our assembler output.  */

static int app_on;

/* If we are outputting an insn sequence, this contains the sequence rtx.
   Zero otherwise.  */

rtx final_sequence;

#ifdef ASSEMBLER_DIALECT

/* Number of the assembler dialect to use, starting at 0.  */
static int dialect_number;
#endif

/* Indexed by line number, nonzero if there is a note for that line.  */

static char *line_note_exists;

/* Linked list to hold line numbers for each basic block.  */

struct bb_list {
  struct bb_list *next;		/* pointer to next basic block */
  int line_num;			/* line number */
  int file_label_num;		/* LPBC<n> label # for stored filename */
  int func_label_num;		/* LPBC<n> label # for stored function name */
};

static struct bb_list *bb_head	= 0;		/* Head of basic block list */
static struct bb_list **bb_tail = &bb_head;	/* Ptr to store next bb ptr */
static int bb_file_label_num	= -1;		/* Current label # for file */
static int bb_func_label_num	= -1;		/* Current label # for func */

/* Linked list to hold the strings for each file and function name output.  */

struct bb_str {
  struct bb_str *next;		/* pointer to next string */
  char *string;			/* string */
  int label_num;		/* label number */
  int length;			/* string length */
};

extern rtx peephole		PROTO((rtx));

static struct bb_str *sbb_head	= 0;		/* Head of string list.  */
static struct bb_str **sbb_tail	= &sbb_head;	/* Ptr to store next bb str */
static int sbb_label_num	= 0;		/* Last label used */

static int asm_insn_count	PROTO((rtx));
static void profile_function	PROTO((FILE *));
static void profile_after_prologue PROTO((FILE *));
static void add_bb		PROTO((FILE *));
static int add_bb_string	PROTO((char *, int));
static void output_source_line	PROTO((FILE *, rtx));
static rtx walk_alter_subreg	PROTO((rtx));
static void output_asm_name	PROTO((void));
static void output_operand	PROTO((rtx, int));
#ifdef LEAF_REGISTERS
static void leaf_renumber_regs	PROTO((rtx));
#endif
#ifdef HAVE_cc0
static int alter_cond		PROTO((rtx));
#endif

extern char *getpwd ();

/* Initialize data in final at the beginning of a compilation.  */

void
init_final (filename)
     char *filename;
{
  next_block_index = 2;
  app_on = 0;
  max_block_depth = 20;
  pending_blocks = (int *) xmalloc (20 * sizeof *pending_blocks);
  final_sequence = 0;

#ifdef ASSEMBLER_DIALECT
  dialect_number = ASSEMBLER_DIALECT;
#endif
}

/* Called at end of source file,
   to output the block-profiling table for this entire compilation.  */

void
end_final (filename)
     char *filename;
{
  int i;

  if (profile_block_flag || profile_arc_flag)
    {
      char name[20];
      int align = exact_log2 (BIGGEST_ALIGNMENT / BITS_PER_UNIT);
      int size, rounded;
      struct bb_list *ptr;
      struct bb_str *sptr;
      int long_bytes = LONG_TYPE_SIZE / BITS_PER_UNIT;
      int pointer_bytes = POINTER_SIZE / BITS_PER_UNIT;

      if (profile_block_flag)
	size = long_bytes * count_basic_blocks;
      else
	size = long_bytes * count_instrumented_arcs;
      rounded = size;

      rounded += (BIGGEST_ALIGNMENT / BITS_PER_UNIT) - 1;
      rounded = (rounded / (BIGGEST_ALIGNMENT / BITS_PER_UNIT)
		 * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));

      data_section ();

      /* Output the main header, of 11 words:
	 0:  1 if this file is initialized, else 0.
	 1:  address of file name (LPBX1).
	 2:  address of table of counts (LPBX2).
	 3:  number of counts in the table.
	 4:  always 0, for compatibility with Sun.

         The following are GNU extensions:

	 5:  address of table of start addrs of basic blocks (LPBX3).
	 6:  Number of bytes in this header.
	 7:  address of table of function names (LPBX4).
	 8:  address of table of line numbers (LPBX5) or 0.
	 9:  address of table of file names (LPBX6) or 0.
	10:  space reserved for basic block profiling.  */

      ASM_OUTPUT_ALIGN (asm_out_file, align);

      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LPBX", 0);
      /* zero word */
      assemble_integer (const0_rtx, long_bytes, 1);

      /* address of filename */
      ASM_GENERATE_INTERNAL_LABEL (name, "LPBX", 1);
      assemble_integer (gen_rtx (SYMBOL_REF, Pmode, name), pointer_bytes, 1);

      /* address of count table */
      ASM_GENERATE_INTERNAL_LABEL (name, "LPBX", 2);
      assemble_integer (gen_rtx (SYMBOL_REF, Pmode, name), pointer_bytes, 1);

      /* count of the # of basic blocks or # of instrumented arcs */
      if (profile_block_flag)
	assemble_integer (GEN_INT (count_basic_blocks), long_bytes, 1);
      else
	assemble_integer (GEN_INT (count_instrumented_arcs), long_bytes,
			  1);

      /* zero word (link field) */
      assemble_integer (const0_rtx, pointer_bytes, 1);

      /* address of basic block start address table */
      if (profile_block_flag)
	{
	  ASM_GENERATE_INTERNAL_LABEL (name, "LPBX", 3);
	  assemble_integer (gen_rtx (SYMBOL_REF, Pmode, name), pointer_bytes,
			    1);
	}
      else
	assemble_integer (const0_rtx, pointer_bytes, 1);

      /* byte count for extended structure.  */
      assemble_integer (GEN_INT (10 * UNITS_PER_WORD), long_bytes, 1);

      /* address of function name table */
      if (profile_block_flag)
	{
	  ASM_GENERATE_INTERNAL_LABEL (name, "LPBX", 4);
	  assemble_integer (gen_rtx (SYMBOL_REF, Pmode, name), pointer_bytes,
			    1);
	}
      else
	assemble_integer (const0_rtx, pointer_bytes, 1);

      /* address of line number and filename tables if debugging.  */
      if (write_symbols != NO_DEBUG && profile_block_flag)
	{
	  ASM_GENERATE_INTERNAL_LABEL (name, "LPBX", 5);
	  assemble_integer (gen_rtx (SYMBOL_REF, Pmode, name), pointer_bytes, 1);
	  ASM_GENERATE_INTERNAL_LABEL (name, "LPBX", 6);
	  assemble_integer (gen_rtx (SYMBOL_REF, Pmode, name), pointer_bytes, 1);
	}
      else
	{
	  assemble_integer (const0_rtx, pointer_bytes, 1);
	  assemble_integer (const0_rtx, pointer_bytes, 1);
	}

      /* space for extension ptr (link field) */
      assemble_integer (const0_rtx, UNITS_PER_WORD, 1);

      /* Output the file name changing the suffix to .d for Sun tcov
	 compatibility.  */
      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LPBX", 1);
      {
	char *cwd = getpwd ();
	int len = strlen (filename) + strlen (cwd) + 1;
	char *data_file = (char *) alloca (len + 4);

	strcpy (data_file, cwd);
	strcat (data_file, "/");
	strcat (data_file, filename);
	strip_off_ending (data_file, len);
	if (profile_block_flag)
	  strcat (data_file, ".d");
	else
	  strcat (data_file, ".da");
	assemble_string (data_file, strlen (data_file) + 1);
      }

      /* Make space for the table of counts.  */
      if (size == 0)
	{
	  /* Realign data section.  */
	  ASM_OUTPUT_ALIGN (asm_out_file, align);
	  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LPBX", 2);
	  if (size != 0)
	    assemble_zeros (size);
	}
      else
	{
	  ASM_GENERATE_INTERNAL_LABEL (name, "LPBX", 2);
#ifdef ASM_OUTPUT_SHARED_LOCAL
	  if (flag_shared_data)
	    ASM_OUTPUT_SHARED_LOCAL (asm_out_file, name, size, rounded);
	  else
#endif
#ifdef ASM_OUTPUT_ALIGNED_DECL_LOCAL
	    ASM_OUTPUT_ALIGNED_DECL_LOCAL (asm_out_file, NULL_TREE, name, size,
					      BIGGEST_ALIGNMENT);
#else
#ifdef ASM_OUTPUT_ALIGNED_LOCAL
	    ASM_OUTPUT_ALIGNED_LOCAL (asm_out_file, name, size,
				      BIGGEST_ALIGNMENT);
#else
	    ASM_OUTPUT_LOCAL (asm_out_file, name, size, rounded);
#endif
#endif
	}

      /* Output any basic block strings */
      if (profile_block_flag)
	{
	  readonly_data_section ();
	  if (sbb_head)
	    {
	      ASM_OUTPUT_ALIGN (asm_out_file, align);
	      for (sptr = sbb_head; sptr != 0; sptr = sptr->next)
		{
		  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LPBC",
					     sptr->label_num);
		  assemble_string (sptr->string, sptr->length);
		}
	    }
	}

      /* Output the table of addresses.  */
      if (profile_block_flag)
	{
	  /* Realign in new section */
	  ASM_OUTPUT_ALIGN (asm_out_file, align);
	  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LPBX", 3);
	  for (i = 0; i < count_basic_blocks; i++)
	    {
	      ASM_GENERATE_INTERNAL_LABEL (name, "LPB", i);
	      assemble_integer (gen_rtx (SYMBOL_REF, Pmode, name),
				pointer_bytes, 1);
	    }
	}

      /* Output the table of function names.  */
      if (profile_block_flag)
	{
	  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LPBX", 4);
	  for ((ptr = bb_head), (i = 0); ptr != 0; (ptr = ptr->next), i++)
	    {
	      if (ptr->func_label_num >= 0)
		{
		  ASM_GENERATE_INTERNAL_LABEL (name, "LPBC",
					       ptr->func_label_num);
		  assemble_integer (gen_rtx (SYMBOL_REF, Pmode, name),
				    pointer_bytes, 1);
		}
	      else
		assemble_integer (const0_rtx, pointer_bytes, 1);
	    }

	  for ( ; i < count_basic_blocks; i++)
	    assemble_integer (const0_rtx, pointer_bytes, 1);
	}

      if (write_symbols != NO_DEBUG && profile_block_flag)
	{
	  /* Output the table of line numbers.  */
	  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LPBX", 5);
	  for ((ptr = bb_head), (i = 0); ptr != 0; (ptr = ptr->next), i++)
	    assemble_integer (GEN_INT (ptr->line_num), long_bytes, 1);

	  for ( ; i < count_basic_blocks; i++)
	    assemble_integer (const0_rtx, long_bytes, 1);

	  /* Output the table of file names.  */
	  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LPBX", 6);
	  for ((ptr = bb_head), (i = 0); ptr != 0; (ptr = ptr->next), i++)
	    {
	      if (ptr->file_label_num >= 0)
		{
		  ASM_GENERATE_INTERNAL_LABEL (name, "LPBC",
					       ptr->file_label_num);
		  assemble_integer (gen_rtx (SYMBOL_REF, Pmode, name),
				    pointer_bytes, 1);
		}
	      else
		assemble_integer (const0_rtx, pointer_bytes, 1);
	    }

	  for ( ; i < count_basic_blocks; i++)
	    assemble_integer (const0_rtx, pointer_bytes, 1);
	}

      /* End with the address of the table of addresses,
	 so we can find it easily, as the last word in the file's text.  */
      if (profile_block_flag)
	{
	  ASM_GENERATE_INTERNAL_LABEL (name, "LPBX", 3);
	  assemble_integer (gen_rtx (SYMBOL_REF, Pmode, name), pointer_bytes,
			    1);
	}
    }
}

/* Enable APP processing of subsequent output.
   Used before the output from an `asm' statement.  */

void
app_enable ()
{
  if (! app_on)
    {
      fprintf (asm_out_file, ASM_APP_ON);
      app_on = 1;
    }
}

/* Disable APP processing of subsequent output.
   Called from varasm.c before most kinds of output.  */

void
app_disable ()
{
  if (app_on)
    {
      fprintf (asm_out_file, ASM_APP_OFF);
      app_on = 0;
    }
}

/* Return the number of slots filled in the current 
   delayed branch sequence (we don't count the insn needing the
   delay slot).   Zero if not in a delayed branch sequence.  */

#ifdef DELAY_SLOTS
int
dbr_sequence_length ()
{
  if (final_sequence != 0)
    return XVECLEN (final_sequence, 0) - 1;
  else
    return 0;
}
#endif

/* The next two pages contain routines used to compute the length of an insn
   and to shorten branches.  */

/* Arrays for insn lengths, and addresses.  The latter is referenced by
   `insn_current_length'.  */

static short *insn_lengths;
int *insn_addresses;

/* Address of insn being processed.  Used by `insn_current_length'.  */
int insn_current_address;

/* Indicate that branch shortening hasn't yet been done.  */

void
init_insn_lengths ()
{
  insn_lengths = 0;
}

/* Obtain the current length of an insn.  If branch shortening has been done,
   get its actual length.  Otherwise, get its maximum length.  */

int
get_attr_length (insn)
     rtx insn;
{
#ifdef HAVE_ATTR_length
  rtx body;
  int i;
  int length = 0;

  if (insn_lengths)
    return insn_lengths[INSN_UID (insn)];
  else
    switch (GET_CODE (insn))
      {
      case NOTE:
      case BARRIER:
      case CODE_LABEL:
	return 0;

      case CALL_INSN:
	length = insn_default_length (insn);
	break;

      case JUMP_INSN:
	body = PATTERN (insn);
        if (GET_CODE (body) == ADDR_VEC || GET_CODE (body) == ADDR_DIFF_VEC)
	  {
	    /* This only takes room if jump tables go into the text section.  */
#if !defined(READONLY_DATA_SECTION) || defined(JUMP_TABLES_IN_TEXT_SECTION)
	    length = (XVECLEN (body, GET_CODE (body) == ADDR_DIFF_VEC)
		      * GET_MODE_SIZE (GET_MODE (body)));

	    /* Be pessimistic and assume worst-case alignment.  */
	    length += (GET_MODE_SIZE (GET_MODE (body)) - 1);
#else
	    return 0;
#endif
	  }
	else
	  length = insn_default_length (insn);
	break;

      case INSN:
	body = PATTERN (insn);
	if (GET_CODE (body) == USE || GET_CODE (body) == CLOBBER)
	  return 0;

	else if (GET_CODE (body) == ASM_INPUT || asm_noperands (body) >= 0)
	  length = asm_insn_count (body) * insn_default_length (insn);
	else if (GET_CODE (body) == SEQUENCE)
	  for (i = 0; i < XVECLEN (body, 0); i++)
	    length += get_attr_length (XVECEXP (body, 0, i));
	else
	  length = insn_default_length (insn);
	break;

      default:
	break;
      }

#ifdef ADJUST_INSN_LENGTH
  ADJUST_INSN_LENGTH (insn, length);
#endif
  return length;
#else /* not HAVE_ATTR_length */
  return 0;
#endif /* not HAVE_ATTR_length */
}

/* Make a pass over all insns and compute their actual lengths by shortening
   any branches of variable length if possible.  */

/* Give a default value for the lowest address in a function.  */

#ifndef FIRST_INSN_ADDRESS
#define FIRST_INSN_ADDRESS 0
#endif

void
shorten_branches (first)
     rtx first;
{
#ifdef HAVE_ATTR_length
  rtx insn;
  int something_changed = 1;
  int max_uid = 0;
  char *varying_length;
  rtx body;
  int uid;

  /* In order to make sure that all instructions have valid length info,
     we must split them before we compute the address/length info.  */

  for (insn = NEXT_INSN (first); insn; insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
      insn = try_split (PATTERN (insn), insn, 1);

  /* Compute maximum UID and allocate arrays.  */
  for (insn = first; insn; insn = NEXT_INSN (insn))
    if (INSN_UID (insn) > max_uid)
      max_uid = INSN_UID (insn);

  max_uid++;
  insn_lengths = (short *) oballoc (max_uid * sizeof (short));
  insn_addresses = (int *) oballoc (max_uid * sizeof (int));
  varying_length = (char *) oballoc (max_uid * sizeof (char));

  /* Compute initial lengths, addresses, and varying flags for each insn.  */
  for (insn_current_address = FIRST_INSN_ADDRESS, insn = first;
       insn != 0;
       insn_current_address += insn_lengths[uid], insn = NEXT_INSN (insn))
    {
      uid = INSN_UID (insn);
      insn_addresses[uid] = insn_current_address;
      insn_lengths[uid] = 0;
      varying_length[uid] = 0;
      
      if (GET_CODE (insn) == NOTE || GET_CODE (insn) == BARRIER
	  || GET_CODE (insn) == CODE_LABEL)
	continue;
      if (INSN_DELETED_P (insn))
	continue;

      body = PATTERN (insn);
      if (GET_CODE (body) == ADDR_VEC || GET_CODE (body) == ADDR_DIFF_VEC)
	{
	  /* This only takes room if read-only data goes into the text
	     section.  */
#if !defined(READONLY_DATA_SECTION) || defined(JUMP_TABLES_IN_TEXT_SECTION)
	  int unitsize = GET_MODE_SIZE (GET_MODE (body));

	  insn_lengths[uid] = (XVECLEN (body, GET_CODE (body) == ADDR_DIFF_VEC)
			       * GET_MODE_SIZE (GET_MODE (body)));

	  /* We don't know what address the ADDR_VEC/ADDR_DIFF_VEC will end
	     up at after branch shortening.  As a result, it is impossible
	     to determine how much padding we need at this point.  Therefore,
	     assume worst possible alignment.  */
	  insn_lengths[uid] += unitsize - 1;

#else
	  ;
#endif
	}
      else if (asm_noperands (body) >= 0)
	insn_lengths[uid] = asm_insn_count (body) * insn_default_length (insn);
      else if (GET_CODE (body) == SEQUENCE)
	{
	  int i;
	  int const_delay_slots;
#ifdef DELAY_SLOTS
	  const_delay_slots = const_num_delay_slots (XVECEXP (body, 0, 0));
#else
	  const_delay_slots = 0;
#endif
	  /* Inside a delay slot sequence, we do not do any branch shortening
	     if the shortening could change the number of delay slots
	     of the branch.  */
	  for (i = 0; i < XVECLEN (body, 0); i++)
	    {
	      rtx inner_insn = XVECEXP (body, 0, i);
	      int inner_uid = INSN_UID (inner_insn);
	      int inner_length;

	      if (asm_noperands (PATTERN (XVECEXP (body, 0, i))) >= 0)
		inner_length = (asm_insn_count (PATTERN (inner_insn))
				* insn_default_length (inner_insn));
	      else
		inner_length = insn_default_length (inner_insn);
	      
	      insn_lengths[inner_uid] = inner_length;
	      if (const_delay_slots)
		{
		  if ((varying_length[inner_uid]
		       = insn_variable_length_p (inner_insn)) != 0)
		    varying_length[uid] = 1;
		  insn_addresses[inner_uid] = (insn_current_address +
					       insn_lengths[uid]);
		}
	      else
		varying_length[inner_uid] = 0;
	      insn_lengths[uid] += inner_length;
	    }
	}
      else if (GET_CODE (body) != USE && GET_CODE (body) != CLOBBER)
	{
	  insn_lengths[uid] = insn_default_length (insn);
	  varying_length[uid] = insn_variable_length_p (insn);
	}

      /* If needed, do any adjustment.  */
#ifdef ADJUST_INSN_LENGTH
      ADJUST_INSN_LENGTH (insn, insn_lengths[uid]);
#endif
    }

  /* Now loop over all the insns finding varying length insns.  For each,
     get the current insn length.  If it has changed, reflect the change.
     When nothing changes for a full pass, we are done.  */

  while (something_changed)
    {
      something_changed = 0;
      for (insn_current_address = FIRST_INSN_ADDRESS, insn = first;
	   insn != 0;
	   insn = NEXT_INSN (insn))
	{
	  int new_length;
	  int tmp_length;

	  uid = INSN_UID (insn);
	  insn_addresses[uid] = insn_current_address;
	  if (! varying_length[uid])
	    {
	      insn_current_address += insn_lengths[uid];
	      continue;
	    }
	  if (GET_CODE (insn) == INSN && GET_CODE (PATTERN (insn)) == SEQUENCE)
	    {
	      int i;
	      
	      body = PATTERN (insn);
	      new_length = 0;
	      for (i = 0; i < XVECLEN (body, 0); i++)
		{
		  rtx inner_insn = XVECEXP (body, 0, i);
		  int inner_uid = INSN_UID (inner_insn);
		  int inner_length;

		  insn_addresses[inner_uid] = insn_current_address;

		  /* insn_current_length returns 0 for insns with a
		     non-varying length.  */
		  if (! varying_length[inner_uid])
		    inner_length = insn_lengths[inner_uid];
		  else
		    inner_length = insn_current_length (inner_insn);

		  if (inner_length != insn_lengths[inner_uid])
		    {
		      insn_lengths[inner_uid] = inner_length;
		      something_changed = 1;
		    }
		  insn_current_address += insn_lengths[inner_uid];
		  new_length += inner_length;
		}
	    }
	  else
	    {
	      new_length = insn_current_length (insn);
	      insn_current_address += new_length;
	    }

#ifdef SHORTEN_WITH_ADJUST_INSN_LENGTH
#ifdef ADJUST_INSN_LENGTH
	  /* If needed, do any adjustment.  */
	  tmp_length = new_length;
	  ADJUST_INSN_LENGTH (insn, new_length);
	  insn_current_address += (new_length - tmp_length);
#endif
#endif

	  if (new_length != insn_lengths[uid])
	    {
	      insn_lengths[uid] = new_length;
	      something_changed = 1;
	    }
	}
      /* For a non-optimizing compile, do only a single pass.  */
      if (!optimize)
	break;
    }
#endif /* HAVE_ATTR_length */
}

#ifdef HAVE_ATTR_length
/* Given the body of an INSN known to be generated by an ASM statement, return
   the number of machine instructions likely to be generated for this insn.
   This is used to compute its length.  */

static int
asm_insn_count (body)
     rtx body;
{
  char *template;
  int count = 1;

  if (GET_CODE (body) == ASM_INPUT)
    template = XSTR (body, 0);
  else
    template = decode_asm_operands (body, NULL_PTR, NULL_PTR,
				    NULL_PTR, NULL_PTR);

  for ( ; *template; template++)
    if (IS_ASM_LOGICAL_LINE_SEPARATOR(*template) || *template == '\n')
      count++;

  return count;
}
#endif

/* Output assembler code for the start of a function,
   and initialize some of the variables in this file
   for the new function.  The label for the function and associated
   assembler pseudo-ops have already been output in `assemble_start_function'.

   FIRST is the first insn of the rtl for the function being compiled.
   FILE is the file to write assembler code to.
   OPTIMIZE is nonzero if we should eliminate redundant
     test and compare insns.  */

void
final_start_function (first, file, optimize)
     rtx first;
     FILE *file;
     int optimize;
{
  block_depth = 0;

  this_is_asm_operands = 0;

#ifdef NON_SAVING_SETJMP
  /* A function that calls setjmp should save and restore all the
     call-saved registers on a system where longjmp clobbers them.  */
  if (NON_SAVING_SETJMP && current_function_calls_setjmp)
    {
      int i;

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (!call_used_regs[i] && !call_fixed_regs[i])
	  regs_ever_live[i] = 1;
    }
#endif
  
  /* Initial line number is supposed to be output
     before the function's prologue and label
     so that the function's address will not appear to be
     in the last statement of the preceding function.  */
  if (NOTE_LINE_NUMBER (first) != NOTE_INSN_DELETED)
    last_linenum = high_block_linenum = high_function_linenum
      = NOTE_LINE_NUMBER (first);

#if defined (DWARF2_UNWIND_INFO) || defined (DWARF2_DEBUGGING_INFO)
  /* Output DWARF definition of the function.  */
  if (dwarf2out_do_frame ())
    dwarf2out_begin_prologue ();
#endif

  /* For SDB and XCOFF, the function beginning must be marked between
     the function label and the prologue.  We always need this, even when
     -g1 was used.  Defer on MIPS systems so that parameter descriptions
     follow function entry.  */
#if defined(SDB_DEBUGGING_INFO) && !defined(MIPS_DEBUGGING_INFO)
  if (write_symbols == SDB_DEBUG)
    sdbout_begin_function (last_linenum);
  else
#endif
#ifdef XCOFF_DEBUGGING_INFO
    if (write_symbols == XCOFF_DEBUG)
      xcoffout_begin_function (file, last_linenum);
    else
#endif	  
      /* But only output line number for other debug info types if -g2
	 or better.  */
      if (NOTE_LINE_NUMBER (first) != NOTE_INSN_DELETED)
	output_source_line (file, first);

#ifdef LEAF_REG_REMAP
  if (leaf_function)
    leaf_renumber_regs (first);
#endif

  /* The Sun386i and perhaps other machines don't work right
     if the profiling code comes after the prologue.  */
#ifdef PROFILE_BEFORE_PROLOGUE
  if (profile_flag)
    profile_function (file);
#endif /* PROFILE_BEFORE_PROLOGUE */

#if defined (DWARF2_UNWIND_INFO) && defined (HAVE_prologue)
  if (dwarf2out_do_frame ())
    dwarf2out_frame_debug (NULL_RTX);
#endif

#ifdef FUNCTION_PROLOGUE
  /* First output the function prologue: code to set up the stack frame.  */
  FUNCTION_PROLOGUE (file, get_frame_size ());
#endif

#if defined (SDB_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
  if (write_symbols == SDB_DEBUG || write_symbols == XCOFF_DEBUG)
    next_block_index = 1;
#endif

  /* If the machine represents the prologue as RTL, the profiling code must
     be emitted when NOTE_INSN_PROLOGUE_END is scanned.  */
#ifdef HAVE_prologue
  if (! HAVE_prologue)
#endif
    profile_after_prologue (file);

  profile_label_no++;

  /* If we are doing basic block profiling, remember a printable version
     of the function name.  */
  if (profile_block_flag)
    {
      bb_func_label_num
	= add_bb_string ((*decl_printable_name) (current_function_decl, 2), FALSE);
    }
}

static void
profile_after_prologue (file)
     FILE *file;
{
#ifdef FUNCTION_BLOCK_PROFILER
  if (profile_block_flag)
    {
      FUNCTION_BLOCK_PROFILER (file, count_basic_blocks);
    }
#endif /* FUNCTION_BLOCK_PROFILER */

#ifndef PROFILE_BEFORE_PROLOGUE
  if (profile_flag)
    profile_function (file);
#endif /* not PROFILE_BEFORE_PROLOGUE */
}

static void
profile_function (file)
     FILE *file;
{
  int align = MIN (BIGGEST_ALIGNMENT, LONG_TYPE_SIZE);
  int sval = current_function_returns_struct;
  int cxt = current_function_needs_context;

  data_section ();
  ASM_OUTPUT_ALIGN (file, floor_log2 (align / BITS_PER_UNIT));
  ASM_OUTPUT_INTERNAL_LABEL (file, "LP", profile_label_no);
  assemble_integer (const0_rtx, LONG_TYPE_SIZE / BITS_PER_UNIT, 1);

  function_section (current_function_decl);

#if defined(STRUCT_VALUE_INCOMING_REGNUM) && defined(ASM_OUTPUT_REG_PUSH)
  if (sval)
    ASM_OUTPUT_REG_PUSH (file, STRUCT_VALUE_INCOMING_REGNUM);
#else
#if defined(STRUCT_VALUE_REGNUM) && defined(ASM_OUTPUT_REG_PUSH)
  if (sval)
    ASM_OUTPUT_REG_PUSH (file, STRUCT_VALUE_REGNUM);
#endif
#endif

#if defined(STATIC_CHAIN_INCOMING_REGNUM) && defined(ASM_OUTPUT_REG_PUSH)
  if (cxt)
    ASM_OUTPUT_REG_PUSH (file, STATIC_CHAIN_INCOMING_REGNUM);
#else
#if defined(STATIC_CHAIN_REGNUM) && defined(ASM_OUTPUT_REG_PUSH)
  if (cxt)
    ASM_OUTPUT_REG_PUSH (file, STATIC_CHAIN_REGNUM);
#endif
#endif

  FUNCTION_PROFILER (file, profile_label_no);

#if defined(STATIC_CHAIN_INCOMING_REGNUM) && defined(ASM_OUTPUT_REG_PUSH)
  if (cxt)
    ASM_OUTPUT_REG_POP (file, STATIC_CHAIN_INCOMING_REGNUM);
#else
#if defined(STATIC_CHAIN_REGNUM) && defined(ASM_OUTPUT_REG_PUSH)
  if (cxt)
    ASM_OUTPUT_REG_POP (file, STATIC_CHAIN_REGNUM);
#endif
#endif

#if defined(STRUCT_VALUE_INCOMING_REGNUM) && defined(ASM_OUTPUT_REG_PUSH)
  if (sval)
    ASM_OUTPUT_REG_POP (file, STRUCT_VALUE_INCOMING_REGNUM);
#else
#if defined(STRUCT_VALUE_REGNUM) && defined(ASM_OUTPUT_REG_PUSH)
  if (sval)
    ASM_OUTPUT_REG_POP (file, STRUCT_VALUE_REGNUM);
#endif
#endif
}

/* Output assembler code for the end of a function.
   For clarity, args are same as those of `final_start_function'
   even though not all of them are needed.  */

void
final_end_function (first, file, optimize)
     rtx first;
     FILE *file;
     int optimize;
{
  if (app_on)
    {
      fprintf (file, ASM_APP_OFF);
      app_on = 0;
    }

#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG)
    sdbout_end_function (high_function_linenum);
#endif

#ifdef DWARF_DEBUGGING_INFO
  if (write_symbols == DWARF_DEBUG)
    dwarfout_end_function ();
#endif

#ifdef XCOFF_DEBUGGING_INFO
  if (write_symbols == XCOFF_DEBUG)
    xcoffout_end_function (file, high_function_linenum);
#endif

#ifdef FUNCTION_EPILOGUE
  /* Finally, output the function epilogue:
     code to restore the stack frame and return to the caller.  */
  FUNCTION_EPILOGUE (file, get_frame_size ());
#endif

#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG)
    sdbout_end_epilogue ();
#endif

#ifdef DWARF_DEBUGGING_INFO
  if (write_symbols == DWARF_DEBUG)
    dwarfout_end_epilogue ();
#endif

#if defined (DWARF2_UNWIND_INFO) || defined (DWARF2_DEBUGGING_INFO)
  if (dwarf2out_do_frame ())
    dwarf2out_end_epilogue ();
#endif

#ifdef XCOFF_DEBUGGING_INFO
  if (write_symbols == XCOFF_DEBUG)
    xcoffout_end_epilogue (file);
#endif

  bb_func_label_num = -1;	/* not in function, nuke label # */

  /* If FUNCTION_EPILOGUE is not defined, then the function body
     itself contains return instructions wherever needed.  */
}

/* Add a block to the linked list that remembers the current line/file/function
   for basic block profiling.  Emit the label in front of the basic block and
   the instructions that increment the count field.  */

static void
add_bb (file)
     FILE *file;
{
  struct bb_list *ptr = (struct bb_list *) permalloc (sizeof (struct bb_list));

  /* Add basic block to linked list.  */
  ptr->next = 0;
  ptr->line_num = last_linenum;
  ptr->file_label_num = bb_file_label_num;
  ptr->func_label_num = bb_func_label_num;
  *bb_tail = ptr;
  bb_tail = &ptr->next;

  /* Enable the table of basic-block use counts
     to point at the code it applies to.  */
  ASM_OUTPUT_INTERNAL_LABEL (file, "LPB", count_basic_blocks);

  /* Before first insn of this basic block, increment the
     count of times it was entered.  */
#ifdef BLOCK_PROFILER
  BLOCK_PROFILER (file, count_basic_blocks);
#endif
#ifdef HAVE_cc0
  CC_STATUS_INIT;
#endif

  new_block = 0;
  count_basic_blocks++;
}

/* Add a string to be used for basic block profiling.  */

static int
add_bb_string (string, perm_p)
     char *string;
     int perm_p;
{
  int len;
  struct bb_str *ptr = 0;

  if (!string)
    {
      string = "<unknown>";
      perm_p = TRUE;
    }

  /* Allocate a new string if the current string isn't permanent.  If
     the string is permanent search for the same string in other
     allocations.  */

  len = strlen (string) + 1;
  if (!perm_p)
    {
      char *p = (char *) permalloc (len);
      bcopy (string, p, len);
      string = p;
    }
  else
    for (ptr = sbb_head; ptr != (struct bb_str *) 0; ptr = ptr->next)
      if (ptr->string == string)
	break;

  /* Allocate a new string block if we need to.  */
  if (!ptr)
    {
      ptr = (struct bb_str *) permalloc (sizeof (*ptr));
      ptr->next = 0;
      ptr->length = len;
      ptr->label_num = sbb_label_num++;
      ptr->string = string;
      *sbb_tail = ptr;
      sbb_tail = &ptr->next;
    }

  return ptr->label_num;
}


/* Output assembler code for some insns: all or part of a function.
   For description of args, see `final_start_function', above.

   PRESCAN is 1 if we are not really outputting,
     just scanning as if we were outputting.
   Prescanning deletes and rearranges insns just like ordinary output.
   PRESCAN is -2 if we are outputting after having prescanned.
   In this case, don't try to delete or rearrange insns
   because that has already been done.
   Prescanning is done only on certain machines.  */

void
final (first, file, optimize, prescan)
     rtx first;
     FILE *file;
     int optimize;
     int prescan;
{
  register rtx insn;
  int max_line = 0;

  last_ignored_compare = 0;
  new_block = 1;

  check_exception_handler_labels ();

  /* Make a map indicating which line numbers appear in this function.
     When producing SDB debugging info, delete troublesome line number
     notes from inlined functions in other files as well as duplicate
     line number notes.  */
#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG)
    {
      rtx last = 0;
      for (insn = first; insn; insn = NEXT_INSN (insn))
	if (GET_CODE (insn) == NOTE && NOTE_LINE_NUMBER (insn) > 0)
	  {
	    if ((RTX_INTEGRATED_P (insn)
		 && strcmp (NOTE_SOURCE_FILE (insn), main_input_filename) != 0)
		 || (last != 0
		     && NOTE_LINE_NUMBER (insn) == NOTE_LINE_NUMBER (last)
		     && NOTE_SOURCE_FILE (insn) == NOTE_SOURCE_FILE (last)))
	      {
		NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
		NOTE_SOURCE_FILE (insn) = 0;
		continue;
	      }
	    last = insn;
	    if (NOTE_LINE_NUMBER (insn) > max_line)
	      max_line = NOTE_LINE_NUMBER (insn);
	  }
    }
  else
#endif
    {
      for (insn = first; insn; insn = NEXT_INSN (insn))
	if (GET_CODE (insn) == NOTE && NOTE_LINE_NUMBER (insn) > max_line)
	  max_line = NOTE_LINE_NUMBER (insn);
    }

  line_note_exists = (char *) oballoc (max_line + 1);
  bzero (line_note_exists, max_line + 1);

  for (insn = first; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == NOTE && NOTE_LINE_NUMBER (insn) > 0)
      line_note_exists[NOTE_LINE_NUMBER (insn)] = 1;

  init_recog ();

  CC_STATUS_INIT;

  /* Output the insns.  */
  for (insn = NEXT_INSN (first); insn;)
    {
#ifdef HAVE_ATTR_length
      insn_current_address = insn_addresses[INSN_UID (insn)];
#endif
      insn = final_scan_insn (insn, file, optimize, prescan, 0);
    }

  /* Do basic-block profiling here
     if the last insn was a conditional branch.  */
  if (profile_block_flag && new_block)
    add_bb (file);
}

/* The final scan for one insn, INSN.
   Args are same as in `final', except that INSN
   is the insn being scanned.
   Value returned is the next insn to be scanned.

   NOPEEPHOLES is the flag to disallow peephole processing (currently
   used for within delayed branch sequence output).  */

rtx
final_scan_insn (insn, file, optimize, prescan, nopeepholes)
     rtx insn;
     FILE *file;
     int optimize;
     int prescan;
     int nopeepholes;
{
  register int i;
  insn_counter++;

  /* Ignore deleted insns.  These can occur when we split insns (due to a
     template of "#") while not optimizing.  */
  if (INSN_DELETED_P (insn))
    return NEXT_INSN (insn);

  switch (GET_CODE (insn))
    {
    case NOTE:
      if (prescan > 0)
	break;

      /* Align the beginning of a loop, for higher speed
	 on certain machines.  */

      if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG && optimize > 0)
	{
#ifdef ASM_OUTPUT_LOOP_ALIGN
	  rtx next = next_nonnote_insn (insn);
	  if (next && GET_CODE (next) == CODE_LABEL)
	    {
	      ASM_OUTPUT_LOOP_ALIGN (asm_out_file);
	    }
#endif
	  break;
	}
      if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END)
	break;

      if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_BEG)
	{
	  ASM_OUTPUT_INTERNAL_LABEL (file, "LEHB", NOTE_BLOCK_NUMBER (insn));
	  add_eh_table_entry (NOTE_BLOCK_NUMBER (insn));
#ifdef ASM_OUTPUT_EH_REGION_BEG
	  ASM_OUTPUT_EH_REGION_BEG (file, NOTE_BLOCK_NUMBER (insn));
#endif
	  break;
	}

      if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_EH_REGION_END)
	{
	  ASM_OUTPUT_INTERNAL_LABEL (file, "LEHE", NOTE_BLOCK_NUMBER (insn));
#ifdef ASM_OUTPUT_EH_REGION_END
	  ASM_OUTPUT_EH_REGION_END (file, NOTE_BLOCK_NUMBER (insn));
#endif
	  break;
	}

      if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_PROLOGUE_END)
	{
#ifdef FUNCTION_END_PROLOGUE
	  FUNCTION_END_PROLOGUE (file);
#endif
	  profile_after_prologue (file);
	  break;
	}

#ifdef FUNCTION_BEGIN_EPILOGUE
      if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_EPILOGUE_BEG)
	{
	  FUNCTION_BEGIN_EPILOGUE (file);
	  break;
	}
#endif

      if (write_symbols == NO_DEBUG)
	break;
      if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_FUNCTION_BEG)
	{
#if defined(SDB_DEBUGGING_INFO) && defined(MIPS_DEBUGGING_INFO)
	  /* MIPS stabs require the parameter descriptions to be after the
	     function entry point rather than before.  */
	  if (write_symbols == SDB_DEBUG)
	    sdbout_begin_function (last_linenum);
	  else
#endif
#ifdef DWARF_DEBUGGING_INFO
	  /* This outputs a marker where the function body starts, so it
	     must be after the prologue.  */
	  if (write_symbols == DWARF_DEBUG)
	    dwarfout_begin_function ();
#endif
	  break;
	}
      if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_DELETED)
	break;			/* An insn that was "deleted" */
      if (app_on)
	{
	  fprintf (file, ASM_APP_OFF);
	  app_on = 0;
	}
      if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_BEG
	  && (debug_info_level == DINFO_LEVEL_NORMAL
	      || debug_info_level == DINFO_LEVEL_VERBOSE
	      || write_symbols == DWARF_DEBUG
	      || write_symbols == DWARF2_DEBUG))
	{
	  /* Beginning of a symbol-block.  Assign it a sequence number
	     and push the number onto the stack PENDING_BLOCKS.  */

	  if (block_depth == max_block_depth)
	    {
	      /* PENDING_BLOCKS is full; make it longer.  */
	      max_block_depth *= 2;
	      pending_blocks
		= (int *) xrealloc (pending_blocks,
				    max_block_depth * sizeof (int));
	    }
	  pending_blocks[block_depth++] = next_block_index;

	  high_block_linenum = last_linenum;

	  /* Output debugging info about the symbol-block beginning.  */

#ifdef SDB_DEBUGGING_INFO
	  if (write_symbols == SDB_DEBUG)
	    sdbout_begin_block (file, last_linenum, next_block_index);
#endif
#ifdef XCOFF_DEBUGGING_INFO
	  if (write_symbols == XCOFF_DEBUG)
	    xcoffout_begin_block (file, last_linenum, next_block_index);
#endif
#ifdef DBX_DEBUGGING_INFO
	  if (write_symbols == DBX_DEBUG)
	    ASM_OUTPUT_INTERNAL_LABEL (file, "LBB", next_block_index);
#endif
#ifdef DWARF_DEBUGGING_INFO
	  if (write_symbols == DWARF_DEBUG)
	    dwarfout_begin_block (next_block_index);
#endif
#ifdef DWARF2_DEBUGGING_INFO
	  if (write_symbols == DWARF2_DEBUG)
	    dwarf2out_begin_block (next_block_index);
#endif

	  next_block_index++;
	}
      else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END
	       && (debug_info_level == DINFO_LEVEL_NORMAL
		   || debug_info_level == DINFO_LEVEL_VERBOSE
	           || write_symbols == DWARF_DEBUG
	           || write_symbols == DWARF2_DEBUG))
	{
	  /* End of a symbol-block.  Pop its sequence number off
	     PENDING_BLOCKS and output debugging info based on that.  */

	  --block_depth;

#ifdef XCOFF_DEBUGGING_INFO
	  if (write_symbols == XCOFF_DEBUG && block_depth >= 0)
	    xcoffout_end_block (file, high_block_linenum,
				pending_blocks[block_depth]);
#endif
#ifdef DBX_DEBUGGING_INFO
	  if (write_symbols == DBX_DEBUG && block_depth >= 0)
	    ASM_OUTPUT_INTERNAL_LABEL (file, "LBE",
				       pending_blocks[block_depth]);
#endif
#ifdef SDB_DEBUGGING_INFO
	  if (write_symbols == SDB_DEBUG && block_depth >= 0)
	    sdbout_end_block (file, high_block_linenum,
			      pending_blocks[block_depth]);
#endif
#ifdef DWARF_DEBUGGING_INFO
	  if (write_symbols == DWARF_DEBUG && block_depth >= 0)
	    dwarfout_end_block (pending_blocks[block_depth]);
#endif
#ifdef DWARF2_DEBUGGING_INFO
	  if (write_symbols == DWARF2_DEBUG && block_depth >= 0)
	    dwarf2out_end_block (pending_blocks[block_depth]);
#endif
	}
      else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_DELETED_LABEL
	       && (debug_info_level == DINFO_LEVEL_NORMAL
		   || debug_info_level == DINFO_LEVEL_VERBOSE))
	{
#ifdef DWARF_DEBUGGING_INFO
          if (write_symbols == DWARF_DEBUG)
            dwarfout_label (insn);
#endif
#ifdef DWARF2_DEBUGGING_INFO
          if (write_symbols == DWARF2_DEBUG)
            dwarf2out_label (insn);
#endif
	}
      else if (NOTE_LINE_NUMBER (insn) > 0)
	/* This note is a line-number.  */
	{
	  register rtx note;

#if 0 /* This is what we used to do.  */
	  output_source_line (file, insn);
#endif
	  int note_after = 0;

	  /* If there is anything real after this note,
	     output it.  If another line note follows, omit this one.  */
	  for (note = NEXT_INSN (insn); note; note = NEXT_INSN (note))
	    {
	      if (GET_CODE (note) != NOTE && GET_CODE (note) != CODE_LABEL)
		break;
	      /* These types of notes can be significant
		 so make sure the preceding line number stays.  */
	      else if (GET_CODE (note) == NOTE
		       && (NOTE_LINE_NUMBER (note) == NOTE_INSN_BLOCK_BEG
			   || NOTE_LINE_NUMBER (note) == NOTE_INSN_BLOCK_END
			   || NOTE_LINE_NUMBER (note) == NOTE_INSN_FUNCTION_BEG))
  		break;
	      else if (GET_CODE (note) == NOTE && NOTE_LINE_NUMBER (note) > 0)
		{
		  /* Another line note follows; we can delete this note
		     if no intervening line numbers have notes elsewhere.  */
		  int num;
		  for (num = NOTE_LINE_NUMBER (insn) + 1;
		       num < NOTE_LINE_NUMBER (note);
		       num++)
		    if (line_note_exists[num])
		      break;

		  if (num >= NOTE_LINE_NUMBER (note))
		    note_after = 1;
		  break;
		}
	    }

	  /* Output this line note
	     if it is the first or the last line note in a row.  */
	  if (!note_after)
	    output_source_line (file, insn);
	}
      break;

    case BARRIER:
#ifdef ASM_OUTPUT_ALIGN_CODE
      /* Don't litter the assembler output with needless alignments.  A
	 BARRIER will be placed at the end of every function if HAVE_epilogue
	 is true.  */	 
      if (NEXT_INSN (insn))
	ASM_OUTPUT_ALIGN_CODE (file);
#endif
#if defined (DWARF2_UNWIND_INFO) && !defined (ACCUMULATE_OUTGOING_ARGS)
	/* If we push arguments, we need to check all insns for stack
	   adjustments.  */
	if (dwarf2out_do_frame ())
	  dwarf2out_frame_debug (insn);
#endif
      break;

    case CODE_LABEL:
      CC_STATUS_INIT;
      if (prescan > 0)
	break;
      new_block = 1;

#ifdef FINAL_PRESCAN_LABEL
      FINAL_PRESCAN_INSN (insn, NULL_PTR, 0);
#endif

#ifdef SDB_DEBUGGING_INFO
      if (write_symbols == SDB_DEBUG && LABEL_NAME (insn))
	sdbout_label (insn);
#endif
#ifdef DWARF_DEBUGGING_INFO
      if (write_symbols == DWARF_DEBUG && LABEL_NAME (insn))
	dwarfout_label (insn);
#endif
#ifdef DWARF2_DEBUGGING_INFO
      if (write_symbols == DWARF2_DEBUG && LABEL_NAME (insn))
	dwarf2out_label (insn);
#endif
      if (app_on)
	{
	  fprintf (file, ASM_APP_OFF);
	  app_on = 0;
	}
      if (NEXT_INSN (insn) != 0
	  && GET_CODE (NEXT_INSN (insn)) == JUMP_INSN)
	{
	  rtx nextbody = PATTERN (NEXT_INSN (insn));

	  /* If this label is followed by a jump-table,
	     make sure we put the label in the read-only section.  Also
	     possibly write the label and jump table together.  */

	  if (GET_CODE (nextbody) == ADDR_VEC
	      || GET_CODE (nextbody) == ADDR_DIFF_VEC)
	    {
#ifndef JUMP_TABLES_IN_TEXT_SECTION
	      readonly_data_section ();
#ifdef READONLY_DATA_SECTION
	      ASM_OUTPUT_ALIGN (file,
				exact_log2 (BIGGEST_ALIGNMENT
					    / BITS_PER_UNIT));
#endif /* READONLY_DATA_SECTION */
#else /* JUMP_TABLES_IN_TEXT_SECTION */
	      function_section (current_function_decl);
#endif /* JUMP_TABLES_IN_TEXT_SECTION */
#ifdef ASM_OUTPUT_CASE_LABEL
	      ASM_OUTPUT_CASE_LABEL (file, "L", CODE_LABEL_NUMBER (insn),
				     NEXT_INSN (insn));
#else
	      ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (insn));
#endif
	      break;
	    }
	}

      ASM_OUTPUT_INTERNAL_LABEL (file, "L", CODE_LABEL_NUMBER (insn));
      break;

    default:
      {
	register rtx body = PATTERN (insn), set;
	int insn_code_number;
	char *template;
	rtx note;

	/* An INSN, JUMP_INSN or CALL_INSN.
	   First check for special kinds that recog doesn't recognize.  */

	if (GET_CODE (body) == USE /* These are just declarations */
	    || GET_CODE (body) == CLOBBER)
	  break;

#ifdef HAVE_cc0
	/* If there is a REG_CC_SETTER note on this insn, it means that
	   the setting of the condition code was done in the delay slot
	   of the insn that branched here.  So recover the cc status
	   from the insn that set it.  */

	note = find_reg_note (insn, REG_CC_SETTER, NULL_RTX);
	if (note)
	  {
	    NOTICE_UPDATE_CC (PATTERN (XEXP (note, 0)), XEXP (note, 0));
	    cc_prev_status = cc_status;
	  }
#endif

	/* Detect insns that are really jump-tables
	   and output them as such.  */

	if (GET_CODE (body) == ADDR_VEC || GET_CODE (body) == ADDR_DIFF_VEC)
	  {
	    register int vlen, idx;

	    if (prescan > 0)
	      break;

	    if (app_on)
	      {
		fprintf (file, ASM_APP_OFF);
		app_on = 0;
	      }

	    vlen = XVECLEN (body, GET_CODE (body) == ADDR_DIFF_VEC);
	    for (idx = 0; idx < vlen; idx++)
	      {
		if (GET_CODE (body) == ADDR_VEC)
		  {
#ifdef ASM_OUTPUT_ADDR_VEC_ELT
		    ASM_OUTPUT_ADDR_VEC_ELT
		      (file, CODE_LABEL_NUMBER (XEXP (XVECEXP (body, 0, idx), 0)));
#else
		    abort ();
#endif
		  }
		else
		  {
#ifdef ASM_OUTPUT_ADDR_DIFF_ELT
		    ASM_OUTPUT_ADDR_DIFF_ELT
		      (file,
		       CODE_LABEL_NUMBER (XEXP (XVECEXP (body, 1, idx), 0)),
		       CODE_LABEL_NUMBER (XEXP (XEXP (body, 0), 0)));
#else
		    abort ();
#endif
		  }
	      }
#ifdef ASM_OUTPUT_CASE_END
	    ASM_OUTPUT_CASE_END (file,
				 CODE_LABEL_NUMBER (PREV_INSN (insn)),
				 insn);
#endif

	    function_section (current_function_decl);

	    break;
	  }

	/* Do basic-block profiling when we reach a new block.
	   Done here to avoid jump tables.  */
	if (profile_block_flag && new_block)
	  add_bb (file);

	if (GET_CODE (body) == ASM_INPUT)
	  {
	    /* There's no telling what that did to the condition codes.  */
	    CC_STATUS_INIT;
	    if (prescan > 0)
	      break;
	    if (! app_on)
	      {
		fprintf (file, ASM_APP_ON);
		app_on = 1;
	      }
	    fprintf (asm_out_file, "\t%s\n", XSTR (body, 0));
	    break;
	  }

	/* Detect `asm' construct with operands.  */
	if (asm_noperands (body) >= 0)
	  {
	    int noperands = asm_noperands (body);
	    rtx *ops = (rtx *) alloca (noperands * sizeof (rtx));
	    char *string;

	    /* There's no telling what that did to the condition codes.  */
	    CC_STATUS_INIT;
	    if (prescan > 0)
	      break;

	    if (! app_on)
	      {
		fprintf (file, ASM_APP_ON);
		app_on = 1;
	      }

	    /* Get out the operand values.  */
	    string = decode_asm_operands (body, ops, NULL_PTR,
					  NULL_PTR, NULL_PTR);
	    /* Inhibit aborts on what would otherwise be compiler bugs.  */
	    insn_noperands = noperands;
	    this_is_asm_operands = insn;

	    /* Output the insn using them.  */
	    output_asm_insn (string, ops);
	    this_is_asm_operands = 0;
	    break;
	  }

	if (prescan <= 0 && app_on)
	  {
	    fprintf (file, ASM_APP_OFF);
	    app_on = 0;
	  }

	if (GET_CODE (body) == SEQUENCE)
	  {
	    /* A delayed-branch sequence */
	    register int i;
	    rtx next;

	    if (prescan > 0)
	      break;
	    final_sequence = body;

	    /* The first insn in this SEQUENCE might be a JUMP_INSN that will
	       force the restoration of a comparison that was previously
	       thought unnecessary.  If that happens, cancel this sequence
	       and cause that insn to be restored.  */

	    next = final_scan_insn (XVECEXP (body, 0, 0), file, 0, prescan, 1);
	    if (next != XVECEXP (body, 0, 1))
	      {
		final_sequence = 0;
		return next;
	      }

	    for (i = 1; i < XVECLEN (body, 0); i++)
	      {
		rtx insn = XVECEXP (body, 0, i);
		rtx next = NEXT_INSN (insn);
		/* We loop in case any instruction in a delay slot gets
		   split.  */
		do
		  insn = final_scan_insn (insn, file, 0, prescan, 1);
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
	    if (GET_CODE (XVECEXP (body, 0, 0)) == CALL_INSN)
	      CC_STATUS_INIT;

	    /* Following a conditional branch sequence, we have a new basic
	       block.  */
	    if (profile_block_flag)
	      {
		rtx insn = XVECEXP (body, 0, 0);
		rtx body = PATTERN (insn);

		if ((GET_CODE (insn) == JUMP_INSN && GET_CODE (body) == SET
		     && GET_CODE (SET_SRC (body)) != LABEL_REF)
		    || (GET_CODE (insn) == JUMP_INSN
			&& GET_CODE (body) == PARALLEL
			&& GET_CODE (XVECEXP (body, 0, 0)) == SET
			&& GET_CODE (SET_SRC (XVECEXP (body, 0, 0))) != LABEL_REF))
		  new_block = 1;
	      }
	    break;
	  }

	/* We have a real machine instruction as rtl.  */

	body = PATTERN (insn);

#ifdef HAVE_cc0
	set = single_set(insn);

	/* Check for redundant test and compare instructions
	   (when the condition codes are already set up as desired).
	   This is done only when optimizing; if not optimizing,
	   it should be possible for the user to alter a variable
	   with the debugger in between statements
	   and the next statement should reexamine the variable
	   to compute the condition codes.  */

	if (optimize)
	  {
#if 0
	    rtx set = single_set(insn);
#endif

	    if (set
		&& GET_CODE (SET_DEST (set)) == CC0
		&& insn != last_ignored_compare)
	      {
		if (GET_CODE (SET_SRC (set)) == SUBREG)
		  SET_SRC (set) = alter_subreg (SET_SRC (set));
		else if (GET_CODE (SET_SRC (set)) == COMPARE)
		  {
		    if (GET_CODE (XEXP (SET_SRC (set), 0)) == SUBREG)
		      XEXP (SET_SRC (set), 0)
			= alter_subreg (XEXP (SET_SRC (set), 0));
		    if (GET_CODE (XEXP (SET_SRC (set), 1)) == SUBREG)
		      XEXP (SET_SRC (set), 1)
			= alter_subreg (XEXP (SET_SRC (set), 1));
		  }
		if ((cc_status.value1 != 0
		     && rtx_equal_p (SET_SRC (set), cc_status.value1))
		    || (cc_status.value2 != 0
			&& rtx_equal_p (SET_SRC (set), cc_status.value2)))
		  {
		    /* Don't delete insn if it has an addressing side-effect.  */
		    if (! FIND_REG_INC_NOTE (insn, 0)
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
#endif

	/* Following a conditional branch, we have a new basic block.
	   But if we are inside a sequence, the new block starts after the
	   last insn of the sequence.  */
	if (profile_block_flag && final_sequence == 0
	    && ((GET_CODE (insn) == JUMP_INSN && GET_CODE (body) == SET
		 && GET_CODE (SET_SRC (body)) != LABEL_REF)
		|| (GET_CODE (insn) == JUMP_INSN && GET_CODE (body) == PARALLEL
		    && GET_CODE (XVECEXP (body, 0, 0)) == SET
		    && GET_CODE (SET_SRC (XVECEXP (body, 0, 0))) != LABEL_REF)))
	  new_block = 1;

#ifndef STACK_REGS
	/* Don't bother outputting obvious no-ops, even without -O.
	   This optimization is fast and doesn't interfere with debugging.
	   Don't do this if the insn is in a delay slot, since this
	   will cause an improper number of delay insns to be written.  */
	if (final_sequence == 0
	    && prescan >= 0
	    && GET_CODE (insn) == INSN && GET_CODE (body) == SET
	    && GET_CODE (SET_SRC (body)) == REG
	    && GET_CODE (SET_DEST (body)) == REG
	    && REGNO (SET_SRC (body)) == REGNO (SET_DEST (body)))
	  break;
#endif

#ifdef HAVE_cc0
	/* If this is a conditional branch, maybe modify it
	   if the cc's are in a nonstandard state
	   so that it accomplishes the same thing that it would
	   do straightforwardly if the cc's were set up normally.  */

	if (cc_status.flags != 0
	    && GET_CODE (insn) == JUMP_INSN
	    && GET_CODE (body) == SET
	    && SET_DEST (body) == pc_rtx
	    && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE
	    && GET_RTX_CLASS (GET_CODE (XEXP (SET_SRC (body), 0))) == '<'
	    && XEXP (XEXP (SET_SRC (body), 0), 0) == cc0_rtx
	    /* This is done during prescan; it is not done again
	       in final scan when prescan has been done.  */
	    && prescan >= 0)
	  {
	    /* This function may alter the contents of its argument
	       and clear some of the cc_status.flags bits.
	       It may also return 1 meaning condition now always true
	       or -1 meaning condition now always false
	       or 2 meaning condition nontrivial but altered.  */
	    register int result = alter_cond (XEXP (SET_SRC (body), 0));
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
		PUT_CODE (insn, NOTE);
		NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
		NOTE_SOURCE_FILE (insn) = 0;
		break;
	      }
	    else if (GET_CODE (SET_SRC (body)) == RETURN)
	      /* Replace (set (pc) (return)) with (return).  */
	      PATTERN (insn) = body = SET_SRC (body);

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
	    
	    if (GET_CODE (insn) != JUMP_INSN
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
	    
	    switch (GET_CODE (cond_rtx))
	      {
	      case GTU:
	      case GT:
	      case LTU:
	      case LT:
	      case GEU:
	      case GE:
	      case LEU:
	      case LE:
	      case EQ:
	      case NE:
		{
		  register int result;
		  if (XEXP (cond_rtx, 0) != cc0_rtx)
		    break;
		  result = alter_cond (cond_rtx);
		  if (result == 1)
		    validate_change (insn, &SET_SRC (set), then_rtx, 0);
		  else if (result == -1)
		    validate_change (insn, &SET_SRC (set), else_rtx, 0);
		  else if (result == 2)
		    INSN_CODE (insn) = -1;
		  if (SET_DEST (set) == SET_SRC (set))
		    {
		      PUT_CODE (insn, NOTE);
		      NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
		      NOTE_SOURCE_FILE (insn) = 0;
		    }
		}
		break;

	      default:
		break;
	      }
	  }

#endif

	/* Do machine-specific peephole optimizations if desired.  */

	if (optimize && !flag_no_peephole && !nopeepholes)
	  {
	    rtx next = peephole (insn);
	    /* When peepholing, if there were notes within the peephole,
	       emit them before the peephole.  */
	    if (next != 0 && next != NEXT_INSN (insn))
	      {
		rtx prev = PREV_INSN (insn);
		rtx note;

		for (note = NEXT_INSN (insn); note != next;
		     note = NEXT_INSN (note))
		  final_scan_insn (note, file, optimize, prescan, nopeepholes);

		/* In case this is prescan, put the notes
		   in proper position for later rescan.  */
		note = NEXT_INSN (insn);
		PREV_INSN (note) = prev;
		NEXT_INSN (prev) = note;
		NEXT_INSN (PREV_INSN (next)) = insn;
		PREV_INSN (insn) = PREV_INSN (next);
		NEXT_INSN (insn) = next;
		PREV_INSN (next) = insn;
	      }

	    /* PEEPHOLE might have changed this.  */
	    body = PATTERN (insn);
	  }

	/* Try to recognize the instruction.
	   If successful, verify that the operands satisfy the
	   constraints for the instruction.  Crash if they don't,
	   since `reload' should have changed them so that they do.  */

	insn_code_number = recog_memoized (insn);
	insn_extract (insn);
	for (i = 0; i < insn_n_operands[insn_code_number]; i++)
	  {
	    if (GET_CODE (recog_operand[i]) == SUBREG)
	      recog_operand[i] = alter_subreg (recog_operand[i]);
	    else if (GET_CODE (recog_operand[i]) == PLUS
		     || GET_CODE (recog_operand[i]) == MULT)
	      recog_operand[i] = walk_alter_subreg (recog_operand[i]);
	  }

	for (i = 0; i < insn_n_dups[insn_code_number]; i++)
	  {
	    if (GET_CODE (*recog_dup_loc[i]) == SUBREG)
	      *recog_dup_loc[i] = alter_subreg (*recog_dup_loc[i]);
	    else if (GET_CODE (*recog_dup_loc[i]) == PLUS
		     || GET_CODE (*recog_dup_loc[i]) == MULT)
	      *recog_dup_loc[i] = walk_alter_subreg (*recog_dup_loc[i]);
	  }

#ifdef REGISTER_CONSTRAINTS
	if (! constrain_operands (insn_code_number, 1))
	  fatal_insn_not_found (insn);
#endif

	/* Some target machines need to prescan each insn before
	   it is output.  */

#ifdef FINAL_PRESCAN_INSN
	FINAL_PRESCAN_INSN (insn, recog_operand,
			    insn_n_operands[insn_code_number]);
#endif

#ifdef HAVE_cc0
	cc_prev_status = cc_status;

	/* Update `cc_status' for this instruction.
	   The instruction's output routine may change it further.
	   If the output routine for a jump insn needs to depend
	   on the cc status, it should look at cc_prev_status.  */

	NOTICE_UPDATE_CC (body, insn);
#endif

	debug_insn = insn;

	/* If the proper template needs to be chosen by some C code,
	   run that code and get the real template.  */

	template = insn_template[insn_code_number];
	if (template == 0)
	  {
	    template = (*insn_outfun[insn_code_number]) (recog_operand, insn);

	    /* If the C code returns 0, it means that it is a jump insn
	       which follows a deleted test insn, and that test insn
	       needs to be reinserted.  */
	    if (template == 0)
	      {
		if (prev_nonnote_insn (insn) != last_ignored_compare)
		  abort ();
		new_block = 0;
		return prev_nonnote_insn (insn);
	      }
	  }

	/* If the template is the string "#", it means that this insn must
	   be split.  */
	if (template[0] == '#' && template[1] == '\0')
	  {
	    rtx new = try_split (body, insn, 0);

	    /* If we didn't split the insn, go away.  */
	    if (new == insn && PATTERN (new) == body)
	      abort ();
	      
#ifdef HAVE_ATTR_length
	    /* This instruction should have been split in shorten_branches,
	       to ensure that we would have valid length info for the
	       splitees.  */
	    abort ();
#endif

	    new_block = 0;
	    return new;
	  }
	
	if (prescan > 0)
	  break;

	/* Output assembler code from the template.  */

	output_asm_insn (template, recog_operand);

#if defined (DWARF2_UNWIND_INFO)
#if !defined (ACCUMULATE_OUTGOING_ARGS)
	/* If we push arguments, we need to check all insns for stack
	   adjustments.  */
	if (dwarf2out_do_frame ())
	  dwarf2out_frame_debug (insn);
#else
#if defined (HAVE_prologue)
	/* If this insn is part of the prologue, emit DWARF v2
	   call frame info.  */
	if (RTX_FRAME_RELATED_P (insn) && dwarf2out_do_frame ())
	  dwarf2out_frame_debug (insn);
#endif
#endif
#endif

#if 0
	/* It's not at all clear why we did this and doing so interferes
	   with tests we'd like to do to use REG_WAS_0 notes, so let's try
	   with this out.  */

	/* Mark this insn as having been output.  */
	INSN_DELETED_P (insn) = 1;
#endif

	debug_insn = 0;
      }
    }
  return NEXT_INSN (insn);
}

/* Output debugging info to the assembler file FILE
   based on the NOTE-insn INSN, assumed to be a line number.  */

static void
output_source_line (file, insn)
     FILE *file;
     rtx insn;
{
  register char *filename = NOTE_SOURCE_FILE (insn);

  /* Remember filename for basic block profiling.
     Filenames are allocated on the permanent obstack
     or are passed in ARGV, so we don't have to save
     the string.  */

  if (profile_block_flag && last_filename != filename)
    bb_file_label_num = add_bb_string (filename, TRUE);

  last_filename = filename;
  last_linenum = NOTE_LINE_NUMBER (insn);
  high_block_linenum = MAX (last_linenum, high_block_linenum);
  high_function_linenum = MAX (last_linenum, high_function_linenum);

  if (write_symbols != NO_DEBUG)
    {
#ifdef SDB_DEBUGGING_INFO
      if (write_symbols == SDB_DEBUG
#if 0 /* People like having line numbers even in wrong file!  */
	  /* COFF can't handle multiple source files--lose, lose.  */
	  && !strcmp (filename, main_input_filename)
#endif
	  /* COFF relative line numbers must be positive.  */
	  && last_linenum > sdb_begin_function_line)
	{
#ifdef ASM_OUTPUT_SOURCE_LINE
	  ASM_OUTPUT_SOURCE_LINE (file, last_linenum);
#else
	  fprintf (file, "\t.ln\t%d\n",
		   ((sdb_begin_function_line > -1)
		    ? last_linenum - sdb_begin_function_line : 1));
#endif
	}
#endif

#if defined (DBX_DEBUGGING_INFO)
      if (write_symbols == DBX_DEBUG)
	dbxout_source_line (file, filename, NOTE_LINE_NUMBER (insn));
#endif

#if defined (XCOFF_DEBUGGING_INFO)
      if (write_symbols == XCOFF_DEBUG)
	xcoffout_source_line (file, filename, insn);
#endif

#ifdef DWARF_DEBUGGING_INFO
      if (write_symbols == DWARF_DEBUG)
	dwarfout_line (filename, NOTE_LINE_NUMBER (insn));
#endif

#ifdef DWARF2_DEBUGGING_INFO
      if (write_symbols == DWARF2_DEBUG)
	dwarf2out_line (filename, NOTE_LINE_NUMBER (insn));
#endif
    }
}

/* If X is a SUBREG, replace it with a REG or a MEM,
   based on the thing it is a subreg of.  */

rtx
alter_subreg (x)
     register rtx x;
{
  register rtx y = SUBREG_REG (x);
  if (GET_CODE (y) == SUBREG)
    y = alter_subreg (y);

  if (GET_CODE (y) == REG)
    {
      /* If the containing reg really gets a hard reg, so do we.  */
      PUT_CODE (x, REG);
      REGNO (x) = REGNO (y) + SUBREG_WORD (x);
    }
  else if (GET_CODE (y) == MEM)
    {
      register int offset = SUBREG_WORD (x) * UNITS_PER_WORD;
      if (BYTES_BIG_ENDIAN)
	offset -= (MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (x)))
		   - MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (y))));
      PUT_CODE (x, MEM);
      MEM_VOLATILE_P (x) = MEM_VOLATILE_P (y);
      XEXP (x, 0) = plus_constant (XEXP (y, 0), offset);
    }

  return x;
}

/* Do alter_subreg on all the SUBREGs contained in X.  */

static rtx
walk_alter_subreg (x)
     rtx x;
{
  switch (GET_CODE (x))
    {
    case PLUS:
    case MULT:
      XEXP (x, 0) = walk_alter_subreg (XEXP (x, 0));
      XEXP (x, 1) = walk_alter_subreg (XEXP (x, 1));
      break;

    case MEM:
      XEXP (x, 0) = walk_alter_subreg (XEXP (x, 0));
      break;

    case SUBREG:
      return alter_subreg (x);
      
    default:
      break;
    }

  return x;
}

#ifdef HAVE_cc0

/* Given BODY, the body of a jump instruction, alter the jump condition
   as required by the bits that are set in cc_status.flags.
   Not all of the bits there can be handled at this level in all cases.

   The value is normally 0.
   1 means that the condition has become always true.
   -1 means that the condition has become always false.
   2 means that COND has been altered.  */

static int
alter_cond (cond)
     register rtx cond;
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
	abort ();

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
output_operand_lossage (str)
     char *str;
{
  if (this_is_asm_operands)
    error_for_asm (this_is_asm_operands, "invalid `asm': %s", str);
  else
    abort ();
}

/* Output of assembler code from a template, and its subroutines.  */

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

static void
output_asm_name ()
{
  if (flag_print_asm_name)
    {
      /* Annotate the assembly with a comment describing the pattern and
	 alternative used.  */
      if (debug_insn)
	{
	  register int num = INSN_CODE (debug_insn);
	  fprintf (asm_out_file, " %s %d %s", 
		   ASM_COMMENT_START, INSN_UID (debug_insn), insn_name[num]);
	  if (insn_n_alternatives[num] > 1)
	    fprintf (asm_out_file, "/%d", which_alternative + 1);

	  /* Clear this so only the first assembler insn
	     of any rtl insn will get the special comment for -dp.  */
	  debug_insn = 0;
	}
    }
}

void
output_asm_insn (template, operands)
     char *template;
     rtx *operands;
{
  register char *p;
  register int c, i;

  /* An insn may return a null string template
     in a case where no assembler code is needed.  */
  if (*template == 0)
    return;

  p = template;
  putc ('\t', asm_out_file);

#ifdef ASM_OUTPUT_OPCODE
  ASM_OUTPUT_OPCODE (asm_out_file, p);
#endif

  while (c = *p++)
    switch (c)
      {
      case '\n':
	output_asm_name ();
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
	/* If we want the first dialect, do nothing.  Otherwise, skip
	   DIALECT_NUMBER of strings ending with '|'.  */
	for (i = 0; i < dialect_number; i++)
	  {
	    while (*p && *p++ != '|')
	      ;

	    if (*p == '|')
	      p++;
	  }
	break;

      case '|':
	/* Skip to close brace.  */
	while (*p && *p++ != '}')
	  ;
	break;

      case '}':
	break;
#endif

      case '%':
	/* %% outputs a single %.  */
	if (*p == '%')
	  {
	    p++;
	    putc (c, asm_out_file);
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
	   the PRINT_OPERAND macro can define them.  */
	else if ((*p >= 'a' && *p <= 'z')
		 || (*p >= 'A' && *p <= 'Z'))
	  {
	    int letter = *p++;
	    c = atoi (p);

	    if (! (*p >= '0' && *p <= '9'))
	      output_operand_lossage ("operand number missing after %-letter");
	    else if (this_is_asm_operands && c >= (unsigned) insn_noperands)
	      output_operand_lossage ("operand number out of range");
	    else if (letter == 'l')
	      output_asm_label (operands[c]);
	    else if (letter == 'a')
	      output_address (operands[c]);
	    else if (letter == 'c')
	      {
		if (CONSTANT_ADDRESS_P (operands[c]))
		  output_addr_const (asm_out_file, operands[c]);
		else
		  output_operand (operands[c], 'c');
	      }
	    else if (letter == 'n')
	      {
		if (GET_CODE (operands[c]) == CONST_INT)
		  fprintf (asm_out_file, HOST_WIDE_INT_PRINT_DEC,
			   - INTVAL (operands[c]));
		else
		  {
		    putc ('-', asm_out_file);
		    output_addr_const (asm_out_file, operands[c]);
		  }
	      }
	    else
	      output_operand (operands[c], letter);
	    
	    while ((c = *p) >= '0' && c <= '9') p++;
	  }
	/* % followed by a digit outputs an operand the default way.  */
	else if (*p >= '0' && *p <= '9')
	  {
	    c = atoi (p);
	    if (this_is_asm_operands && c >= (unsigned) insn_noperands)
	      output_operand_lossage ("operand number out of range");
	    else
	      output_operand (operands[c], 0);
	    while ((c = *p) >= '0' && c <= '9') p++;
	  }
	/* % followed by punctuation: output something for that
	   punctuation character alone, with no operand.
	   The PRINT_OPERAND macro decides what is actually done.  */
#ifdef PRINT_OPERAND_PUNCT_VALID_P
	else if (PRINT_OPERAND_PUNCT_VALID_P (*p))
	  output_operand (NULL_RTX, *p++);
#endif
	else
	  output_operand_lossage ("invalid %%-code");
	break;

      default:
	putc (c, asm_out_file);
      }

  output_asm_name ();

  putc ('\n', asm_out_file);
}

/* Output a LABEL_REF, or a bare CODE_LABEL, as an assembler symbol.  */

void
output_asm_label (x)
     rtx x;
{
  char buf[256];

  if (GET_CODE (x) == LABEL_REF)
    ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (XEXP (x, 0)));
  else if (GET_CODE (x) == CODE_LABEL)
    ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (x));
  else
    output_operand_lossage ("`%l' operand isn't a label");

  assemble_name (asm_out_file, buf);
}

/* Print operand X using machine-dependent assembler syntax.
   The macro PRINT_OPERAND is defined just to control this function.
   CODE is a non-digit that preceded the operand-number in the % spec,
   such as 'z' if the spec was `%z3'.  CODE is 0 if there was no char
   between the % and the digits.
   When CODE is a non-letter, X is 0.

   The meanings of the letters are machine-dependent and controlled
   by PRINT_OPERAND.  */

static void
output_operand (x, code)
     rtx x;
     int code;
{
  if (x && GET_CODE (x) == SUBREG)
    x = alter_subreg (x);

  /* If X is a pseudo-register, abort now rather than writing trash to the
     assembler file.  */

  if (x && GET_CODE (x) == REG && REGNO (x) >= FIRST_PSEUDO_REGISTER)
    abort ();

  PRINT_OPERAND (asm_out_file, x, code);
}

/* Print a memory reference operand for address X
   using machine-dependent assembler syntax.
   The macro PRINT_OPERAND_ADDRESS exists just to control this function.  */

void
output_address (x)
     rtx x;
{
  walk_alter_subreg (x);
  PRINT_OPERAND_ADDRESS (asm_out_file, x);
}

/* Print an integer constant expression in assembler syntax.
   Addition and subtraction are the only arithmetic
   that may appear in these expressions.  */

void
output_addr_const (file, x)
     FILE *file;
     rtx x;
{
  char buf[256];

 restart:
  switch (GET_CODE (x))
    {
    case PC:
      if (flag_pic)
	putc ('.', file);
      else
	abort ();
      break;

    case SYMBOL_REF:
      assemble_name (file, XSTR (x, 0));
      break;

    case LABEL_REF:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (XEXP (x, 0)));
      assemble_name (file, buf);
      break;

    case CODE_LABEL:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (x));
      assemble_name (file, buf);
      break;

    case CONST_INT:
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
      break;

    case CONST:
      /* This used to output parentheses around the expression,
	 but that does not work on the 386 (either ATT or BSD assembler).  */
      output_addr_const (file, XEXP (x, 0));
      break;

    case CONST_DOUBLE:
      if (GET_MODE (x) == VOIDmode)
	{
	  /* We can use %d if the number is one word and positive.  */
	  if (CONST_DOUBLE_HIGH (x))
	    fprintf (file, HOST_WIDE_INT_PRINT_DOUBLE_HEX,
		     CONST_DOUBLE_HIGH (x), CONST_DOUBLE_LOW (x));
	  else if  (CONST_DOUBLE_LOW (x) < 0)
	    fprintf (file, HOST_WIDE_INT_PRINT_HEX, CONST_DOUBLE_LOW (x));
	  else
	    fprintf (file, HOST_WIDE_INT_PRINT_DEC, CONST_DOUBLE_LOW (x));
	}
      else
	/* We can't handle floating point constants;
	   PRINT_OPERAND must handle them.  */
	output_operand_lossage ("floating constant misused");
      break;

    case PLUS:
      /* Some assemblers need integer constants to appear last (eg masm).  */
      if (GET_CODE (XEXP (x, 0)) == CONST_INT)
	{
	  output_addr_const (file, XEXP (x, 1));
	  if (INTVAL (XEXP (x, 0)) >= 0)
	    fprintf (file, "+");
	  output_addr_const (file, XEXP (x, 0));
	}
      else
	{
	  output_addr_const (file, XEXP (x, 0));
	  if (INTVAL (XEXP (x, 1)) >= 0)
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
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) < 0)
	{
	  fprintf (file, ASM_OPEN_PAREN);
	  output_addr_const (file, XEXP (x, 1));
	  fprintf (file, ASM_CLOSE_PAREN);
	}
      else
	output_addr_const (file, XEXP (x, 1));
      break;

    case ZERO_EXTEND:
    case SIGN_EXTEND:
      output_addr_const (file, XEXP (x, 0));
      break;

    default:
      output_operand_lossage ("invalid expression as operand");
    }
}

/* A poor man's fprintf, with the added features of %I, %R, %L, and %U.
   %R prints the value of REGISTER_PREFIX.
   %L prints the value of LOCAL_LABEL_PREFIX.
   %U prints the value of USER_LABEL_PREFIX.
   %I prints the value of IMMEDIATE_PREFIX.
   %O runs ASM_OUTPUT_OPCODE to transform what follows in the string.
   Also supported are %d, %x, %s, %e, %f, %g and %%.

   We handle alternate assembler dialects here, just like output_asm_insn.  */

void
asm_fprintf VPROTO((FILE *file, char *p, ...))
{
#ifndef __STDC__
  FILE *file;
  char *p;
#endif
  va_list argptr;
  char buf[10];
  char *q, c;
  int i;

  VA_START (argptr, p);

#ifndef __STDC__
  file = va_arg (argptr, FILE *);
  p = va_arg (argptr, char *);
#endif

  buf[0] = '%';

  while (c = *p++)
    switch (c)
      {
#ifdef ASSEMBLER_DIALECT
      case '{':
	/* If we want the first dialect, do nothing.  Otherwise, skip
	   DIALECT_NUMBER of strings ending with '|'.  */
	for (i = 0; i < dialect_number; i++)
	  {
	    while (*p && *p++ != '|')
	      ;

	    if (*p == '|')
	      p++;
	  }
	break;

      case '|':
	/* Skip to close brace.  */
	while (*p && *p++ != '}')
	  ;
	break;

      case '}':
	break;
#endif

      case '%':
	c = *p++;
	q = &buf[1];
	while ((c >= '0' && c <= '9') || c == '.')
	  {
	    *q++ = c;
	    c = *p++;
	  }
	switch (c)
	  {
	  case '%':
	    fprintf (file, "%%");
	    break;

	  case 'd':  case 'i':  case 'u':
	  case 'x':  case 'p':  case 'X':
	  case 'o':
	    *q++ = c;
	    *q = 0;
	    fprintf (file, buf, va_arg (argptr, int));
	    break;

	  case 'w':
	    /* This is a prefix to the 'd', 'i', 'u', 'x', 'p', and 'X' cases,
	       but we do not check for those cases.  It means that the value
	       is a HOST_WIDE_INT, which may be either `int' or `long'.  */

#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
#else
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
	    *q++ = 'l';
#else
	    *q++ = 'l';
	    *q++ = 'l';
#endif
#endif

	    *q++ = *p++;
	    *q = 0;
	    fprintf (file, buf, va_arg (argptr, HOST_WIDE_INT));
	    break;

	  case 'l':
	    *q++ = c;
	    *q++ = *p++;
	    *q = 0;
	    fprintf (file, buf, va_arg (argptr, long));
	    break;

	  case 'e':
	  case 'f':
	  case 'g':
	    *q++ = c;
	    *q = 0;
	    fprintf (file, buf, va_arg (argptr, double));
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
#ifdef USER_LABEL_PREFIX
	    fprintf (file, "%s", USER_LABEL_PREFIX);
#endif
	    break;

	  default:
	    abort ();
	  }
	break;

      default:
	fputc (c, file);
      }
}

/* Split up a CONST_DOUBLE or integer constant rtx
   into two rtx's for single words,
   storing in *FIRST the word that comes first in memory in the target
   and in *SECOND the other.  */

void
split_double (value, first, second)
     rtx value;
     rtx *first, *second;
{
  if (GET_CODE (value) == CONST_INT)
    {
      if (HOST_BITS_PER_WIDE_INT >= (2 * BITS_PER_WORD))
	{
	  /* In this case the CONST_INT holds both target words.
	     Extract the bits from it into two word-sized pieces.  */
	  rtx low, high;
	  HOST_WIDE_INT word_mask;
	  /* Avoid warnings for shift count >= BITS_PER_WORD.  */
	  int shift_count = BITS_PER_WORD - 1;

	  word_mask = (HOST_WIDE_INT) 1 << shift_count;
	  word_mask |= word_mask - 1;
	  low = GEN_INT (INTVAL (value) & word_mask);
	  high = GEN_INT ((INTVAL (value) >> (shift_count + 1)) & word_mask);
	  if (WORDS_BIG_ENDIAN)
	    {
	      *first = high;
	      *second = low;
	    }
	  else
	    {
	      *first = low;
	      *second = high;
	    }
	}
      else
	{
	  /* The rule for using CONST_INT for a wider mode
	     is that we regard the value as signed.
	     So sign-extend it.  */
	  rtx high = (INTVAL (value) < 0 ? constm1_rtx : const0_rtx);
	  if (WORDS_BIG_ENDIAN)
	    {
	      *first = high;
	      *second = value;
	    }
	  else
	    {
	      *first = value;
	      *second = high;
	    }
	}
    }
  else if (GET_CODE (value) != CONST_DOUBLE)
    {
      if (WORDS_BIG_ENDIAN)
	{
	  *first = const0_rtx;
	  *second = value;
	}
      else
	{
	  *first = value;
	  *second = const0_rtx;
	}
    }
  else if (GET_MODE (value) == VOIDmode
	   /* This is the old way we did CONST_DOUBLE integers.  */
	   || GET_MODE_CLASS (GET_MODE (value)) == MODE_INT)
    {
      /* In an integer, the words are defined as most and least significant.
	 So order them by the target's convention.  */
      if (WORDS_BIG_ENDIAN)
	{
	  *first = GEN_INT (CONST_DOUBLE_HIGH (value));
	  *second = GEN_INT (CONST_DOUBLE_LOW (value));
	}
      else
	{
	  *first = GEN_INT (CONST_DOUBLE_LOW (value));
	  *second = GEN_INT (CONST_DOUBLE_HIGH (value));
	}
    }
  else
    {
#ifdef REAL_ARITHMETIC
      REAL_VALUE_TYPE r; long l[2];
      REAL_VALUE_FROM_CONST_DOUBLE (r, value);

      /* Note, this converts the REAL_VALUE_TYPE to the target's
	 format, splits up the floating point double and outputs
	 exactly 32 bits of it into each of l[0] and l[1] --
	 not necessarily BITS_PER_WORD bits.  */
      REAL_VALUE_TO_TARGET_DOUBLE (r, l);

      *first = GEN_INT ((HOST_WIDE_INT) l[0]);
      *second = GEN_INT ((HOST_WIDE_INT) l[1]);
#else
      if ((HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
	   || HOST_BITS_PER_WIDE_INT != BITS_PER_WORD)
	  && ! flag_pretend_float)
      abort ();

      if (
#ifdef HOST_WORDS_BIG_ENDIAN
	  WORDS_BIG_ENDIAN
#else
	  ! WORDS_BIG_ENDIAN
#endif
	  )
	{
	  /* Host and target agree => no need to swap.  */
	  *first = GEN_INT (CONST_DOUBLE_LOW (value));
	  *second = GEN_INT (CONST_DOUBLE_HIGH (value));
	}
      else
	{
	  *second = GEN_INT (CONST_DOUBLE_LOW (value));
	  *first = GEN_INT (CONST_DOUBLE_HIGH (value));
	}
#endif /* no REAL_ARITHMETIC */
    }
}

/* Return nonzero if this function has no function calls.  */

int
leaf_function_p ()
{
  rtx insn;

  if (profile_flag || profile_block_flag || profile_arc_flag)
    return 0;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == CALL_INSN)
	return 0;
      if (GET_CODE (insn) == INSN
	  && GET_CODE (PATTERN (insn)) == SEQUENCE
	  && GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == CALL_INSN)
	return 0;
    }
  for (insn = current_function_epilogue_delay_list; insn; insn = XEXP (insn, 1))
    {
      if (GET_CODE (XEXP (insn, 0)) == CALL_INSN)
	return 0;
      if (GET_CODE (XEXP (insn, 0)) == INSN
	  && GET_CODE (PATTERN (XEXP (insn, 0))) == SEQUENCE
	  && GET_CODE (XVECEXP (PATTERN (XEXP (insn, 0)), 0, 0)) == CALL_INSN)
	return 0;
    }

  return 1;
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

static char permitted_reg_in_leaf_functions[] = LEAF_REGISTERS;

/* Return 1 if this function uses only the registers that can be
   safely renumbered.  */

int
only_leaf_regs_used ()
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if ((regs_ever_live[i] || global_regs[i])
	  && ! permitted_reg_in_leaf_functions[i])
	return 0;
    }
  return 1;
}

/* Scan all instructions and renumber all registers into those
   available in leaf functions.  */

static void
leaf_renumber_regs (first)
     rtx first;
{
  rtx insn;

  /* Renumber only the actual patterns.
     The reg-notes can contain frame pointer refs,
     and renumbering them could crash, and should not be needed.  */
  for (insn = first; insn; insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
      leaf_renumber_regs_insn (PATTERN (insn));
  for (insn = current_function_epilogue_delay_list; insn; insn = XEXP (insn, 1))
    if (GET_RTX_CLASS (GET_CODE (XEXP (insn, 0))) == 'i')
      leaf_renumber_regs_insn (PATTERN (XEXP (insn, 0)));
}

/* Scan IN_RTX and its subexpressions, and renumber all regs into those
   available in leaf functions.  */

void
leaf_renumber_regs_insn (in_rtx)
     register rtx in_rtx;
{
  register int i, j;
  register char *format_ptr;

  if (in_rtx == 0)
    return;

  /* Renumber all input-registers into output-registers.
     renumbered_regs would be 1 for an output-register;
     they  */

  if (GET_CODE (in_rtx) == REG)
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
      if (newreg < 0)
	abort ();
      regs_ever_live[REGNO (in_rtx)] = 0;
      regs_ever_live[newreg] = 1;
      REGNO (in_rtx) = newreg;
      in_rtx->used = 1;
    }

  if (GET_RTX_CLASS (GET_CODE (in_rtx)) == 'i')
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
	abort ();
      }
}
#endif
