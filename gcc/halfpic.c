/* OSF/rose half-pic support functions.
   Copyright (C) 1992 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* The OSF/rose half-pic model assumes that the non-library code does
   not need to have full PIC (position independent code), but rather,
   that pointers to external references are put into the data section
   and derefenced as normal pointers.  References to static data does
   not need to be PIC-ized.

   Another optimization is to have the compiler know what symbols are
   in the shared libraries, and to only lay down the pointers to
   things which in the library proper.  */

#include "config.h"

#ifdef HALF_PIC_INIT

#include "tree.h"
#include "rtl.h"
#include <stdio.h>
#include "obstack.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern char *xmalloc ();
extern void  free ();
extern rtx eliminate_constant_term ();

int flag_half_pic;		/* Global half-pic flag.  */

/* Obstack to hold generated pic names.  */
static struct obstack half_pic_obstack;

/* List of pointers created to pic references.  */

struct all_refs {
  struct all_refs *next;	/* next name created */
  rtx		   ref;		/* reference rtl */
  char		  *ref_name;	/* reference name to ptr to real_name */
  int		   ref_len;	/* reference name length */
  char		  *real_name;	/* real function/data name */
  int		   real_len;	/* strlen (real_name) */
};

static struct all_refs *half_pic_names;


/* Do any half-pic initializations.  */

void
half_pic_init ()
{
  flag_half_pic = TRUE;
  obstack_init (&half_pic_obstack);
}


/* Write out all pointers to pic references.  */

void
half_pic_finish (stream)
     FILE *stream;
{
  struct all_refs *p = half_pic_names;

  if (!p)
    return;

  data_section ();
  for (; p != 0; p = p->next)
    {
      ASM_OUTPUT_LABEL (stream, p->ref_name);
      ASM_OUTPUT_INT (stream, gen_rtx (SYMBOL_REF, Pmode, p->real_name));
    }
}


/* Encode in a declaration whether or not it is half-pic.  */

void
half_pic_encode (decl)
     tree decl;
{
  enum tree_code code = TREE_CODE (decl);
  tree asm_name;

  if (!flag_half_pic)
    return;

  if (code != VAR_DECL && code != FUNCTION_DECL)
    return;

  /* If this is not an external reference, it can't be half-pic.  */
  if (!TREE_EXTERNAL (decl))
    return;

  asm_name = DECL_ASSEMBLER_NAME (decl);
  if (!asm_name)
    return;

  TREE_PUBLIC (asm_name) = TRUE;

#ifdef HALF_PIC_DEBUG
  if (HALF_PIC_DEBUG)
    fprintf (stderr, "\n========== Half_pic_encode %.*s\n",
	     IDENTIFIER_LENGTH (asm_name),
	     IDENTIFIER_POINTER (asm_name));
#endif
}


/* Return whether an address is half-pic.  */

int
half_pic_address_p (addr)
     rtx addr;
{
  char *name;
  tree tname;

  if (!flag_half_pic)
    return FALSE;

  switch (GET_CODE (addr))
    {
    case CONST:
      {
	rtx offset = const0_rtx;
	addr = eliminate_constant_term (addr, &offset);
	if (GET_CODE (addr) != SYMBOL_REF)
	  return FALSE;
      }
      /* fall through */

    case SYMBOL_REF:
      name = XSTR (addr, 0);

#ifdef HALF_PIC_DEBUG
      if (HALF_PIC_DEBUG)
	fprintf (stderr, "\n========== Half_pic_address_p %s\n", name);
#endif

      /* If this is a label, it will have a '*' in front of it.  */
      if (name[0] == '*')
	return FALSE;

      tname = get_identifier (name);
      if (TREE_PUBLIC (tname))
	{
#ifdef HALF_PIC_DEBUG
	  if (HALF_PIC_DEBUG)
	    fprintf (stderr, "%s is half-pic\n", name);
#endif
	  return TRUE;
	}
    }

  return FALSE;
}


/* Return the name of the pointer to the PIC function, allocating
   it if need be.  */

struct rtx_def *
half_pic_ptr (operand)
     rtx operand;
{
  char *name;
  tree tname;
  struct all_refs *p;
  int ch;
  int len;
  int prefix_len;

  if (GET_CODE (operand) != SYMBOL_REF)
    return operand;

  name = XSTR (operand, 0);
  len = strlen (name);
  ch = name[0];
  for (p = half_pic_names; p != 0; p = p->next)
    {
      if (ch == *(p->ref_name)
	  && len == p->real_len
	  && !strcmp (name, p->real_name))
	return p->ref;
    }

  p = (struct all_refs *) obstack_alloc (&half_pic_obstack, sizeof (struct all_refs));

  prefix_len = strlen (HALF_PIC_PREFIX);
  obstack_grow (&half_pic_obstack, HALF_PIC_PREFIX, prefix_len);
  obstack_grow (&half_pic_obstack, name, len);

  p->next      = half_pic_names;
  p->real_name = name;
  p->real_len  = len;
  p->ref_len   = len + prefix_len;
  p->ref_name  = (char *) obstack_finish (&half_pic_obstack);
  p->ref       = gen_rtx (SYMBOL_REF, Pmode, p->ref_name);

  half_pic_names = p;
}

#endif /* HALF_PIC_INIT */
