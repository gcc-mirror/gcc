/* Output bytecodes for GNU C-compiler.
   Copyright (C) 1993, 1994, 1996, 1997 Free Software Foundation, Inc.

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


#include "config.h"
#include <stdio.h>
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include "machmode.h"
#include "rtl.h"
#include "real.h"
#include "obstack.h"
#include "bytecode.h"
#ifdef __GNUC__
#include "bytetypes.h"
#endif
#include "bc-emit.h"
#include "bc-opcode.h"
#include "bc-typecd.h"
#include "bi-run.h"

extern char *xmalloc (), *xrealloc ();

extern struct obstack *rtl_obstack;

/* Indexed by mode class, gives the narrowest mode for each class.  */

extern enum machine_mode class_narrowest_mode[(int) MAX_MODE_CLASS];

/* Commonly used modes.  */
/* Mode whose width is BITS_PER_UNIT */
extern enum machine_mode byte_mode;

/* Mode whose width is BITS_PER_WORD */
extern enum machine_mode word_mode;

/* Vector indexed by opcode giving info about the args for each opcode.  */
static struct arityvec arityvec[] = {
#include "bc-arity.h"
};

/* How to print a symbol name for the assembler.  */

static void
prsym (file, s)
     FILE *file;
     char *s;
{
  if (*s == '*')
    fprintf (file, "%s", s + 1);
  else

#ifdef NAMES_HAVE_UNDERSCORES
    fprintf (file, "_%s", s);
#else
    fprintf (file, "%s", s);
#endif

}

/* Maintain a bucket hash table for symbol names.  */

#define HASH_BITS 32
#define HASH_SIZE 509

static struct bc_sym *hashtab[HASH_SIZE];

static unsigned int
hash (name)
     char *name;
{
  unsigned int hash = 0;

  while (*name)
    {
      hash = hash << 3 | hash >> HASH_BITS - 3;
      hash += *name++;
    }

  return hash % HASH_SIZE;
}


/* Look up the named symbol, creating it if it doesn't exist.  */

struct bc_sym *
sym_lookup (name)
     char *name;
{
  int i;
  struct bc_sym *s;

  i = hash (name);
  for (s = hashtab[i]; s; s = s->next)
    if (!strcmp (s->name, name))
      return s;

  s = (struct bc_sym *) xmalloc (sizeof (struct bc_sym));
  s->name = xmalloc (strlen (name) + 1);
  strcpy (s->name, name);
  s->defined = s->global = s->common = 0;
  s->val = 0;
  s->next = hashtab[i];
  hashtab[i] = s;
  return s;
}


/* Write out .globl and common symbols to the named file.  */

static void
bc_sym_write (file)
     FILE *file;
{
  int i;
  struct bc_sym *s;

  for (i = 0; i < HASH_SIZE; ++i)
    for (s = hashtab[i]; s; s = s->next)
      {
	if (s->global)
	  {
	    fprintf (file, "\n\t.globl ");
	    prsym (file, s->name);
	    putc ('\n', file);
	    if (s->common)
	      {
		fprintf (file, "\n\t.comm ");
		prsym (file, s->name);
		fprintf (file, ", %lu\n", s->val);
	      }
	  }
	else if (s->common)
	  {
	    fprintf (file, "\n\t.lcomm ");
	    prsym (file, s->name);
	    fprintf (file, ", %lu\n", s->val);
	  }
      }
}




/* Create and initialize a new segment.  */

static struct bc_seg *
seg_create ()
{
  struct bc_seg *result;

  result = (struct bc_seg *) xmalloc (sizeof (struct bc_seg));
  result->alloc = 256;
  result->data = xmalloc (result->alloc);
  result->size = 0;
  result->syms = 0;
  result->relocs = 0;
  return result;
}


/* Advance the segment index to the next alignment boundary.  */

static void
seg_align (seg, log)
     struct bc_seg *seg;
     int log;
{
  unsigned int oldsize = seg->size;

  seg->size = seg->size + (1 << log) - 1 & ~((1 << log) - 1);
  if (seg->size > seg->alloc)
    {
      while (seg->size > seg->alloc)
	seg->alloc *= 2;
      seg->data = xrealloc (seg->data, seg->alloc);
    }
  bzero (seg->data + oldsize, seg->size - oldsize);
}


/* Append the given data to the given segment.  */

static void
seg_data (seg, data, size)
     struct bc_seg *seg;
     char *data;
     unsigned int size;
{
  if (seg->size + size > seg->alloc)
    {
      while (seg->size + size > seg->alloc)
	seg->alloc *= 2;
      seg->data = xrealloc (seg->data, seg->alloc);
    }

  bcopy (data, seg->data + seg->size, size);
  seg->size += size;
}


/* Append a zero-filled skip to the given segment.  */

static void
seg_skip (seg, size)
     struct bc_seg *seg;
     unsigned int size;
{
  if (seg->size + size > seg->alloc)
    {
      while (seg->size + size > seg->alloc)
	seg->alloc *= 2;
      seg->data = xrealloc (seg->data, seg->alloc);
    }

  memset (seg->data + seg->size, 0, size);
  seg->size += size;
}


/* Define the given name as the current offset in the given segment.  It
   is an error if the name is already defined.  Return 0 or 1 indicating
   failure or success respectively.  */

static int
seg_defsym (seg, name)
     struct bc_seg *seg;
     char *name;
{
  struct bc_sym *sym;
  struct bc_segsym *segsym;

  sym = sym_lookup (name);
  if (sym->defined)
    return 0;

  sym->defined = 1;
  sym->val = seg->size;
  segsym = (struct bc_segsym *) xmalloc (sizeof (struct bc_segsym));
  segsym->sym = sym;
  segsym->next = seg->syms;
  seg->syms = segsym;
  return 1;
}


/* Generate in seg's data a reference to the given sym, adjusted by
   the given offset.  */

static void
seg_refsym (seg, name, offset)
     struct bc_seg *seg;
     char *name;
     int offset;
{
  struct bc_sym *sym;
  struct bc_segreloc *segreloc;

  sym = sym_lookup (name);
  segreloc = (struct bc_segreloc *) xmalloc (sizeof (struct bc_segreloc));
  segreloc->offset = seg->size;
  segreloc->sym = sym;
  segreloc->next = seg->relocs;
  seg->relocs = segreloc;
  seg_data (seg, (char *) &offset, sizeof offset);
}


/* Concatenate the contents of given segments into the first argument.  */

static void
seg_concat (result, seg)
     struct bc_seg *result, *seg;
{
  unsigned int fix;
  struct bc_segsym *segsym;
  struct bc_segreloc *segreloc;

  seg_align (result, MACHINE_SEG_ALIGN);
  fix = result->size;
  seg_data (result, seg->data, seg->size);
  free (seg->data);

  /* Go through the symbols and relocs of SEG, adjusting their offsets
     for their new location in RESULT.  */
  if (seg->syms)
    {
      segsym = seg->syms;
      do
	segsym->sym->val += fix;
      while (segsym->next && (segsym = segsym->next));
      segsym->next = result->syms;
      result->syms = seg->syms;
    }
  if (seg->relocs)
    {
      segreloc = seg->relocs;
      do
	segreloc->offset += fix;
      while (segreloc->next && (segreloc = segreloc->next));
      segreloc->next = result->relocs;
      result->relocs = seg->relocs;
    }

  free ((char *) seg);
}

/* Write a segment to a file.  */

static void
bc_seg_write (seg, file)
     struct bc_seg *seg;
     FILE *file;
{
  struct bc_segsym *segsym, *nsegsym, *psegsym;
  struct bc_segreloc *segreloc, *nsegreloc, *psegreloc;
  int i, offset, flag;

  /* Reverse the list of symbols.  */
  for (psegsym = 0, segsym = seg->syms; segsym; segsym = nsegsym)
    {
      nsegsym = segsym->next;
      segsym->next = psegsym;
      psegsym = segsym;
    }
  seg->syms = psegsym;

  /* Reverse the list of relocs.  */
  for (psegreloc = 0, segreloc = seg->relocs; segreloc; segreloc = nsegreloc)
    {
      nsegreloc = segreloc->next;
      segreloc->next = psegreloc;
      psegreloc = segreloc;
    }
  seg->relocs = psegreloc;

  /* Output each byte of the segment.  */
  for (i = 0, segsym = seg->syms, segreloc = seg->relocs; i < seg->size; ++i)
    {
      while (segsym && segsym->sym->val == i)
	{
	  if (i % 8 != 0)
	    putc ('\n', file);

	  BC_WRITE_SEGSYM (segsym, file);
	  segsym = segsym->next;
	  flag = 1;
	}
      if (segreloc && segreloc->offset == i)
	{
	  if (i % 8 != 0)
	    putc ('\n', file);

	  bcopy (seg->data + i, (char *) &offset, sizeof (int));
	  i += sizeof (int) - 1;

	  BC_WRITE_RELOC_ENTRY (segreloc, file, offset);
	  segreloc = segreloc->next;
	  flag = 1;
	}
      else
	{
	  if (i % 8 == 0 || flag)
	    BC_START_BYTECODE_LINE (file);

	  BC_WRITE_BYTECODE (i % 8 == 0 || flag ? ' ' : ',',
			     seg->data[i] & 0xFF,
			     file);
	  flag = 0;
	  if (i % 8 == 7)
	    putc ('\n', file);
	}
    }

  /* Paranoia check--we should have visited all syms and relocs during
     the output pass.  */

  if (segsym || segreloc)
    abort ();
}



/* Text and data segments of the object file in making.  */
static struct bc_seg *bc_text_seg;
static struct bc_seg *bc_data_seg;

/* Called before anything else in this module.  */

void
bc_initialize ()
{
  int min_class_size[(int) MAX_MODE_CLASS];
  enum machine_mode mode;
  int i;

  bc_init_mode_to_code_map ();

  bc_text_seg = seg_create ();
  bc_data_seg = seg_create ();

  dconst0 = REAL_VALUE_ATOF ("0", DFmode);
  dconst1 = REAL_VALUE_ATOF ("1", DFmode);
  dconst2 = REAL_VALUE_ATOF ("2", DFmode);
  dconstm1 = REAL_VALUE_ATOF ("-1", DFmode);

  /* Find the narrowest mode for each class and compute the word and byte
     modes.  */

  for (i = 0; i < (int) MAX_MODE_CLASS; i++)
    min_class_size[i] = 1000;

  for (mode = VOIDmode; (int) mode < (int) MAX_MACHINE_MODE;
       mode = (enum machine_mode) ((int) mode + 1))
    {
      if (GET_MODE_SIZE (mode) < min_class_size[(int) GET_MODE_CLASS (mode)])
	{
	  class_narrowest_mode[(int) GET_MODE_CLASS (mode)] = mode;
	  min_class_size[(int) GET_MODE_CLASS (mode)] = GET_MODE_SIZE (mode);
	}
      if (GET_MODE_CLASS (mode) == MODE_INT
	  && GET_MODE_BITSIZE (mode) == BITS_PER_UNIT)
	byte_mode = mode;

      if (GET_MODE_CLASS (mode) == MODE_INT
	  && GET_MODE_BITSIZE (mode) == BITS_PER_WORD)
	word_mode = mode;
    }
}


/* External addresses referenced in a function.  Rather than trying to
   work relocatable address directly into bytecoded functions (which would
   require us to provide hairy location info and possibly obey alignment
   rules imposed by the architecture) we build an auxiliary table of
   pointer constants, and encode just offsets into this table into the
   actual bytecode.  */
static struct bc_seg *ptrconsts;

/* Trampoline code for the function entry.  */
struct bc_seg *trampoline;

/* Actual byte code of the function.  */
struct bc_seg *bytecode;

/* List of labels defined in the function.  */
struct bc_label *labels;

/* List of label references in the function.  */
struct bc_labelref *labelrefs;


/* Add symbol to pointer table.  Return offset into table where
   pointer was stored.  The offset usually goes into the bytecode
   stream as a constP literal.  */

int
bc_define_pointer (p)
     char *p;
{
  int offset = ptrconsts->size;

  seg_refsym (ptrconsts, p, 0);
  return offset;
}


/* Begin a bytecoded function.  */

int
bc_begin_function (name)
    char *name;
{
  ptrconsts = seg_create ();
  trampoline = seg_create ();
  bytecode = seg_create ();
  return seg_defsym (trampoline, name);
}


/* Force alignment in inline bytecode.  */

void
bc_align_bytecode (align)
    int align;
{
  seg_align (bytecode, align);
}


/* Emit data inline into bytecode.  */

void
bc_emit_bytecode_const (data, size)
     char *data;
     unsigned int size;
{
  if (bytecode)
    seg_data (bytecode, data, size);
}


/* Create a new "bytecode label", to have its value defined later.
   Bytecode labels have nothing to do with the object file symbol table,
   and are purely local to a given bytecoded function.  */

struct bc_label *
bc_get_bytecode_label ()
{
  struct bc_label *result;

  result = (struct bc_label *) xmalloc (sizeof (struct bc_label));
  result->defined = 0;
  result->next = labels;
  result->uid = 0;
  labels = result;
  return result;
}


/* Define the given label with the current location counter.  */

int
bc_emit_bytecode_labeldef (label)
     struct bc_label *label;
{
  extern int bc_new_uid ();

  if (!label || label->defined)
    return 0;

  label->offset = bytecode->size;
  label->defined = 1;
  label->uid = bc_new_uid ();

#ifdef DEBUG_PRINT_CODE
  fprintf (stderr, "$%lx:\n", label);
#endif

  return 1;
}


/* Generate a location-relative reference to the given bytecode label.
   It need not be defined yet; label references will be backpatched later.  */

void
bc_emit_bytecode_labelref (label)
     struct bc_label *label;
{
  struct bc_labelref *labelref;
  static int zero;

  labelref = (struct bc_labelref *) xmalloc (sizeof (struct bc_labelref));
  labelref->label = label;
  labelref->offset = bytecode->size;
  labelref->next = labelrefs;
  labelrefs = labelref;

#ifdef DEBUG_PRINT_CODE
  fprintf (stderr, " $%lx", label);
#endif

  seg_data (bytecode, (char *) &zero, sizeof zero);
}


/* Emit a reference to an external address; generate the reference in the
   ptrconst area, and emit an offset in the bytecode.  */

void
bc_emit_code_labelref (name, offset)
     char *name;
     int offset;
{
  int ptroff;

  ptroff = ptrconsts->size / sizeof (char *);
  seg_data (bytecode, (char *) &ptroff, sizeof ptroff);
  seg_refsym (ptrconsts, name, offset);

#ifdef DEBUG_PRINT_CODE
  fprintf (stderr, " [external <%x> %s]", ptroff, name);
#endif
}


/* Backpatch label references in the byte code, and concatenate the bytecode
   and pointer constant segments to the cumulative text for the object file.
   Return a label name for the pointer constants region.  */

char *
bc_end_function ()
{
  int addr;
  struct bc_label *label, *next;
  struct bc_labelref *ref, *nextref;
  char ptrconsts_label[20];
  static int nlab;

  /* Backpatch bytecode label references.  */
  for (ref = labelrefs; ref; ref = ref->next)
    if (ref->label->defined)
      {
	addr = ref->label->offset;
	bcopy ((char *) &addr, bytecode->data + ref->offset, sizeof addr);
      }

  /* Free the chains of labelrefs and labeldefs.  */
  for (ref = labelrefs; ref; ref = nextref)
    {
      nextref = ref->next;
      free ((char *) ref);
    }

  for (label = labels; label; label = next)
    {
      next = label->next;
      free ((char *) label);
    }

  seg_concat (trampoline, bytecode);
  seg_align (trampoline, MACHINE_SEG_ALIGN);
  sprintf (ptrconsts_label, "*LP%d", nlab++);
  seg_defsym (trampoline, ptrconsts_label);
  seg_concat (trampoline, ptrconsts);
  seg_concat (bc_text_seg, trampoline);

  labels = 0;
  labelrefs = 0;
  trampoline = 0;
  bytecode = 0;
  ptrconsts = 0;

  return sym_lookup (ptrconsts_label)->name;
}

/* Force alignment in const data.  */

void
bc_align_const (align)
     int align;
{
  seg_align (bc_text_seg, align);
}

/* Emit const data.  */

void
bc_emit_const (data, size)
     char *data;
     unsigned int size;
{
  seg_data (bc_text_seg, data, size);
}

/* Emit a zero-filled constant skip.  */

void
bc_emit_const_skip (size)
     unsigned int size;
{
  seg_skip (bc_text_seg, size);
}

/* Emit a label definition in const data.  */

int
bc_emit_const_labeldef (name)
     char *name;
{
  return seg_defsym (bc_text_seg, name);
}

/* Emit a label reference in const data.  */

void
bc_emit_const_labelref (name, offset)
     char *name;
     int offset;
{
  seg_refsym (bc_text_seg, name, offset);
}

/* Force alignment in data.  */

void
bc_align_data (align)
     int align;
{
  seg_align (bc_data_seg, align);
}

/* Emit data.  */

void
bc_emit_data (data, size)
     char *data;
     unsigned int size;
{
  seg_data (bc_data_seg, data, size);
}

/* Emit a zero-filled data skip.  */

void
bc_emit_data_skip (size)
     unsigned int size;
{
  seg_skip (bc_data_seg, size);
}

/* Emit label definition in data.  */

int
bc_emit_data_labeldef (name)
     char *name;
{
  return seg_defsym (bc_data_seg, name);
}

/* Emit label reference in data.  */

void
bc_emit_data_labelref (name, offset)
     char *name;
     int offset;
{
  seg_refsym (bc_data_seg, name, offset);
}

/* Emit a common block of the given name and size.  Note that
   when the .o file is actually written non-global "common"
   blocks will have to be turned into space in the data section.  */

int
bc_emit_common (name, size)
     char *name;
     unsigned int size;
{
  struct bc_sym *sym;

  sym = sym_lookup (name);
  if (sym->defined)
    return 0;

  sym->defined = 1;
  sym->common = 1;
  sym->val = size;
  return 1;
}

/* Globalize the given label.  */

void
bc_globalize_label (name)
     char *name;
{
  struct bc_sym *sym;

  sym = sym_lookup (name);
  sym->global = 1;
}

static enum { in_text, in_data } section = in_text;

void
bc_text ()
{
  section = in_text;
}

void
bc_data ()
{
  section = in_data;
}

void
bc_align (align)
     int align;
{
  if (section == in_text)
    bc_align_const (align);
  else
    bc_align_data (align);
}

void
bc_emit (data, size)
     char *data;
     unsigned int size;
{
  if (section == in_text)
    bc_emit_const (data, size);
  else
    bc_emit_data (data, size);
}

void
bc_emit_skip (size)
     unsigned int size;
{
  if (section == in_text)
    bc_emit_const_skip (size);
  else
    bc_emit_data_skip (size);
}

int
bc_emit_labeldef (name)
     char *name;
{
  if (section == in_text)
    return bc_emit_const_labeldef (name);
  else
    return bc_emit_data_labeldef (name);
}

void
bc_emit_labelref (name, offset)
     char *name;
     int offset;
{
  if (section == in_text)
    bc_emit_const_labelref (name, offset);
  else
    bc_emit_data_labelref (name, offset);
}

void
bc_write_file (file)
     FILE *file;
{
  BC_WRITE_FILE (file);
}


/* Allocate a new bytecode rtx.
   If you supply a null BC_LABEL, we generate one.  */

rtx
bc_gen_rtx (label, offset, bc_label)
     char *label;
     int offset;
     struct bc_label *bc_label;
{
  rtx r;

  if (bc_label == 0)
    bc_label = (struct bc_label *) xmalloc (sizeof (struct bc_label));

  r = gen_rtx (CODE_LABEL, VOIDmode, label, bc_label);
  bc_label->offset = offset;

  return r;
}


/* Print bytecode rtx */

void
bc_print_rtl (fp, r)
     FILE *fp;
     rtx r;
{
#if 0 /* This needs to get fixed to really work again.  */
  /* BC_WRITE_RTL has a definition
     that doesn't even make sense for this use.  */
  BC_WRITE_RTL (r, fp);
#endif
}


/* Emit a bytecode, keeping a running tally of the stack depth.  */

void
bc_emit_bytecode (bytecode)
     enum bytecode_opcode bytecode;
{
  char byte;
  static int prev_lineno = -1;

  byte = (char) bytecode;

#ifdef BCDEBUG_PRINT_CODE
  if (lineno != prev_lineno)
    {
      fprintf (stderr, "<line %d>\n", lineno);
      prev_lineno = lineno;
    }

  fputs (opcode_name[(unsigned int) bytecode], stderr);
#endif

  /* Due to errors we are often requested to output bytecodes that
     will cause an interpreter stack undeflow when executed.  Instead of
     dumping core on such occasions, we omit the bytecode.  Erroneous code
     should not be executed, regardless.  This makes life much easier, since
     we don't have to deceive ourselves about the known stack depth.  */

  bc_emit_bytecode_const (&byte, 1);

  if ((stack_depth -= arityvec[(int) bytecode].ninputs) >= 0)
    {
      if ((stack_depth += arityvec[(int) bytecode].noutputs) > max_stack_depth)
	max_stack_depth = stack_depth;
    }

#ifdef VALIDATE_STACK_FOR_BC
  VALIDATE_STACK_FOR_BC ();
#endif
}


#ifdef BCDEBUG_PRINT_CODE
#define PRLIT(TYPE, PTR)  fprintf (stderr, " [%x]", *(TYPE *) PTR)
#else
#define PRLIT(X,Y)
#endif

/* Emit a complete bytecode instruction, expecting the correct number
   of literal values in the call.  First argument is the instruction, the
   remaining arguments are literals of size HOST_WIDE_INT or smaller.  */

void
bc_emit_instruction VPROTO((enum bytecode_opcode opcode, ...))
{
#ifndef __STDC__
  enum bytecode_opcode opcode;
#endif
  va_list arguments;
  int nliteral, instruction;

  VA_START (arguments, opcode);

#ifndef __STDC__
  opcode = va_arg (arguments, enum bytecode_opcode);
#endif

  /* Emit instruction bytecode */
  bc_emit_bytecode (opcode);
  instruction = (int) opcode;

  /* Loop literals and emit as bytecode constants */
  for (nliteral = 0; nliteral < arityvec[instruction].nliterals; nliteral++)
    {
      switch (arityvec[instruction].literals[nliteral])
	{
/* This conditional is a kludge, but it's necessary
   because TYPE might be long long.  */
#ifdef __GNUC__
	  /* Expand definitions into case statements */
#define DEFTYPECODE(CODE, NAME, MODE, TYPE)				\
	case CODE:							\
	  {								\
	    TYPE temp = va_arg (arguments, TYPE); 			\
	    bc_emit_bytecode_const ((void *) &temp, sizeof temp); 	\
	    PRLIT (TYPE, &temp); }					\
	  break;

#include "bc-typecd.def"

#undef DEFTYPECODE
#endif /* __GNUC__ */

	default:
	  abort ();
	}
    }

  va_end (arguments);

#ifdef BCDEBUG_PRINT_CODE
  fputc ('\n', stderr);
#endif
}

/* Emit the machine-code interface trampoline at the beginning of a byte
   coded function.  The argument is a label name of the interpreter
   bytecode callinfo structure; the return value is a label name for
   the beginning of the actual bytecode.  */

char *
bc_emit_trampoline (callinfo)
     char *callinfo;
{
  char mylab[20];
  static int n;

  sprintf (mylab, "*LB%d", n++);
  
  BC_EMIT_TRAMPOLINE (trampoline, callinfo);

  seg_defsym (bytecode, mylab);
  return sym_lookup (mylab)->name;
}
