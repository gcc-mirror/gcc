/* OSF/rose half-pic support functions.
   Copyright (C) 1992, 1997, 1998 Free Software Foundation, Inc.

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

/* The OSF/rose half-pic model assumes that the non-library code does
   not need to have full PIC (position independent code), but rather,
   that pointers to external references are put into the data section
   and dereferenced as normal pointers.  References to static data does
   not need to be PIC-ized.

   Another optimization is to have the compiler know what symbols are
   in the shared libraries, and to only lay down the pointers to
   things which in the library proper.  */

#include "config.h"

#ifdef HALF_PIC_INIT

#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "obstack.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern rtx eliminate_constant_term ();
extern void assemble_name ();
extern void output_addr_const ();

int flag_half_pic		= 0;	/* Global half-pic flag.  */
int half_pic_number_ptrs	= 0;	/* # distinct pointers found */
int half_pic_number_refs	= 0;	/* # half-pic references */
int (*ptr_half_pic_address_p)() = half_pic_address_p;

/* Obstack to hold generated pic names.  */
static struct obstack half_pic_obstack;

/* List of pointers created to pic references.  */

struct all_refs {
  struct all_refs *hash_next;	/* next name in hash chain */
  struct all_refs *next;	/* next name created */
  int		   external_p;	/* name is an external reference */
  int		   pointer_p;	/* pointer created.  */
  char		  *ref_name;	/* reference name to ptr to real_name */
  int		   ref_len;	/* reference name length */
  char		  *real_name;	/* real function/data name */
  int		   real_len;	/* strlen (real_name) */
};

static struct all_refs *half_pic_names;

static char *half_pic_prefix;
static int   half_pic_prefix_len;


/* Return the hash bucket of a name or NULL.  The hash chain is
   organized as a self reorganizing circularly linked chain.  It is
   assumed that any name passed to use will never be reallocated.  For
   names in SYMBOL_REF's this is true, because the names are allocated
   on the permanent obstack.  */

#ifndef MAX_HASH_TABLE
#define MAX_HASH_TABLE 1009
#endif

#define HASHBITS 30

static struct all_refs *
half_pic_hash (name, len, create_p)
     char *name;		/* name to hash */
     int len;			/* length of the name (or 0 to call strlen) */
     int create_p;		/* != 0 to create new hash bucket if new */
{
  static struct all_refs *hash_table[MAX_HASH_TABLE];
  static struct all_refs  zero_all_refs;

  unsigned char *uname;
  int hash;
  int i;
  int ch;
  struct all_refs *first;
  struct all_refs *ptr;

  if (len == 0)
    len = strlen (name);

  /* Compute hash code */
  uname = (unsigned char *)name;
  ch = uname[0];
  hash = len * 613 + ch;
  for (i = 1; i < len; i += 2)
    hash = (hash * 613) + uname[i];

  hash &= (1 << HASHBITS) - 1;
  hash %= MAX_HASH_TABLE;

  /* See if the name is in the hash table.  */
  ptr = first = hash_table[hash];
  if (ptr)
    {
      do
	{
	  if (len == ptr->real_len
	      && ch == *(ptr->real_name)
	      && !strcmp (name, ptr->real_name))
	    {
	      hash_table[hash] = ptr;
	      return ptr;
	    }

	  ptr = ptr->hash_next;
	}
      while (ptr != first);
    }

  /* name not in hash table.  */
  if (!create_p)
    return (struct all_refs *) 0;

  ptr = (struct all_refs *) obstack_alloc (&half_pic_obstack, sizeof (struct all_refs));
  *ptr = zero_all_refs;

  ptr->real_name = name;
  ptr->real_len  = len;

  /* Update circular links.  */
  if (first == (struct all_refs *) 0)
    ptr->hash_next = ptr;

  else
    {
      ptr->hash_next = first->hash_next;
      first->hash_next = ptr;
    }

  hash_table[hash] = ptr;
  return ptr;
}


/* Do any half-pic initializations.  */

void
half_pic_init ()
{
  flag_half_pic = TRUE;
  half_pic_prefix = HALF_PIC_PREFIX;
  half_pic_prefix_len = strlen (half_pic_prefix);
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
      /* Emit the pointer if used.  */
      if (p->pointer_p)
	{
	  ASM_OUTPUT_LABEL (stream, p->ref_name);
	  ASM_OUTPUT_INT (stream, gen_rtx_SYMBOL_REF (Pmode, p->real_name));
	}
    }
}


/* Encode in a declaration whether or not it is half-pic.  */

void
half_pic_encode (decl)
     tree decl;
{
  enum tree_code code = TREE_CODE (decl);
  tree asm_name;
  struct all_refs *ptr;

  if (!flag_half_pic)
    return;

  if (code != VAR_DECL && code != FUNCTION_DECL)
    return;

  asm_name = DECL_ASSEMBLER_NAME (decl);

  if (!asm_name)
    return;

#ifdef HALF_PIC_DEBUG
  if (HALF_PIC_DEBUG)
    {
      fprintf (stderr, "\n========== Half_pic_encode %.*s\n",
	       IDENTIFIER_LENGTH (asm_name),
	       IDENTIFIER_POINTER (asm_name));
      debug_tree (decl);
    }
#endif

  /* If this is not an external reference, it can't be half-pic.  */
  if (!DECL_EXTERNAL (decl) && (code != VAR_DECL || !TREE_PUBLIC (decl)))
    return;

  ptr = half_pic_hash (IDENTIFIER_POINTER (asm_name),
		       IDENTIFIER_LENGTH (asm_name),
		       TRUE);

  ptr->external_p = TRUE;

#ifdef HALF_PIC_DEBUG
  if (HALF_PIC_DEBUG)
    fprintf (stderr, "\n%.*s is half-pic\n",
	     IDENTIFIER_LENGTH (asm_name),
	     IDENTIFIER_POINTER (asm_name));
#endif
}


/* Mark that an object is now local, and no longer needs half-pic.  */

void
half_pic_declare (name)
     char *name;
{
  struct all_refs *ptr;

  if (!flag_half_pic)
    return;

  ptr = half_pic_hash (name, 0, FALSE);
  if (!ptr)
    return;

  ptr->external_p = FALSE;

#ifdef HALF_PIC_DEBUG
  if (HALF_PIC_DEBUG)
    fprintf (stderr, "\n========== Half_pic_declare %s\n", name);
#endif
}


/* Mark that an object is explicitly external.  */

void
half_pic_external (name)
     char *name;
{
  struct all_refs *ptr;

  if (!flag_half_pic)
    return;

  ptr = half_pic_hash (name, 0, TRUE);
  if (!ptr)
    return;

  ptr->external_p = TRUE;

#ifdef HALF_PIC_DEBUG
  if (HALF_PIC_DEBUG)
    fprintf (stderr, "\n========== Half_pic_external %s\n", name);
#endif
}


/* Return whether an address is half-pic.  */

int
half_pic_address_p (addr)
     rtx addr;
{
  char *name;
  int len;
  struct all_refs *ptr;

  if (!flag_half_pic)
    return FALSE;

  switch (GET_CODE (addr))
    {
    default:
      break;

    case CONST:
      {
	rtx offset = const0_rtx;
	addr = eliminate_constant_term (XEXP (addr, 0), &offset);
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

      /* If this is a reference to the actual half-pic pointer, it
	 is obviously not half-pic.  */

      len = strlen (name);
      if (len > half_pic_prefix_len
	  && half_pic_prefix[0] == name[0]
	  && !strncmp (name, half_pic_prefix, half_pic_prefix_len))
	return FALSE;

      ptr = half_pic_hash (name, len, FALSE);
      if (ptr == (struct all_refs *) 0)
	return FALSE;

      if (ptr->external_p)
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
  struct all_refs *p;
  int len;

  if (GET_CODE (operand) != SYMBOL_REF)
    return operand;

  name = XSTR (operand, 0);
  len = strlen (name);
  p = half_pic_hash (name, len, FALSE);
  if (p == (struct all_refs *) 0 || !p->external_p)
    return operand;

  if (!p->pointer_p)
    {				/* first time, create pointer */
      obstack_grow (&half_pic_obstack, half_pic_prefix, half_pic_prefix_len);
      obstack_grow (&half_pic_obstack, name, len+1);

      p->next      = half_pic_names;
      p->ref_name  = (char *) obstack_finish (&half_pic_obstack);
      p->ref_len   = len + half_pic_prefix_len;
      p->pointer_p = TRUE;

      half_pic_names = p;
      half_pic_number_ptrs++;
    }

  half_pic_number_refs++;
  return gen_rtx_SYMBOL_REF (Pmode, p->ref_name);
}

#endif /* HALF_PIC_INIT */
