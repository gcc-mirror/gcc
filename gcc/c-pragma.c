/* Handle #pragma, system V.4 style.  Supports #pragma weak and #pragma pack.
   Copyright (C) 1992, 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "function.h"
#include "cpplib.h"
#include "c-pragma.h"
#include "flags.h"
#include "toplev.h"
#include "ggc.h"
#include "c-lex.h"
#include "c-common.h"
#include "output.h"
#include "tm_p.h"

#define GCC_BAD(msgid) do { warning (msgid); return; } while (0)
#define GCC_BAD2(msgid, arg) do { warning (msgid, arg); return; } while (0)

#ifdef HANDLE_PRAGMA_PACK
static void handle_pragma_pack PARAMS ((cpp_reader *));

#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
typedef struct align_stack
{
  int                  alignment;
  unsigned int         num_pushes;
  tree                 id;
  struct align_stack * prev;
} align_stack;

static struct align_stack * alignment_stack = NULL;

/* If we have a "global" #pragma pack(<n>) in effect when the first
   #pragma pack(push,<n>) is encountered, this stores the value of 
   maximum_field_alignment in effect.  When the final pop_alignment() 
   happens, we restore the value to this, not to a value of 0 for
   maximum_field_alignment.  Value is in bits.  */
static int default_alignment;
#define SET_GLOBAL_ALIGNMENT(ALIGN) \
  (default_alignment = maximum_field_alignment = (ALIGN))

static void push_alignment PARAMS ((int, tree));
static void pop_alignment  PARAMS ((tree));
static void mark_align_stack PARAMS ((void *));

/* Push an alignment value onto the stack.  */
static void
push_alignment (alignment, id)
     int alignment;
     tree id;
{
  if (alignment_stack == NULL
      || alignment_stack->alignment != alignment
      || id != NULL_TREE)
    {
      align_stack * entry;

      entry = (align_stack *) xmalloc (sizeof (* entry));

      entry->alignment  = alignment;
      entry->num_pushes = 1;
      entry->id         = id;
      entry->prev       = alignment_stack;
      
      /* The current value of maximum_field_alignment is not necessarily 
	 0 since there may be a #pragma pack(<n>) in effect; remember it 
	 so that we can restore it after the final #pragma pop().  */
      if (alignment_stack == NULL)
	default_alignment = maximum_field_alignment;
      
      alignment_stack = entry;

      maximum_field_alignment = alignment;
    }
  else
    alignment_stack->num_pushes ++;
}

/* Undo a push of an alignment onto the stack.  */
static void
pop_alignment (id)
     tree id;
{
  align_stack * entry;
      
  if (alignment_stack == NULL)
    {
      warning ("\
#pragma pack (pop) encountered without matching #pragma pack (push, <n>)"
	       );
      return;
    }

  /* If we got an identifier, strip away everything above the target
     entry so that the next step will restore the state just below it.  */
  if (id)
    {
      for (entry = alignment_stack; entry; entry = entry->prev)
	if (entry->id == id)
	  {
	    entry->num_pushes = 1;
	    alignment_stack = entry;
	    break;
	  }
      if (entry == NULL)
	warning ("\
#pragma pack(pop, %s) encountered without matching #pragma pack(push, %s, <n>)"
		 , IDENTIFIER_POINTER (id), IDENTIFIER_POINTER (id));
    }

  if (-- alignment_stack->num_pushes == 0)
    {
      entry = alignment_stack->prev;

      if (entry == NULL)
	maximum_field_alignment = default_alignment;
      else
	maximum_field_alignment = entry->alignment;

      free (alignment_stack);

      alignment_stack = entry;
    }
}

static void
mark_align_stack (p)
    void *p;
{
  align_stack *a = *(align_stack **) p;

  while (a)
    {
      ggc_mark_tree (a->id);
      a = a->prev;
    }
}
#else  /* not HANDLE_PRAGMA_PACK_PUSH_POP */
#define SET_GLOBAL_ALIGNMENT(ALIGN) (maximum_field_alignment = (ALIGN))
#define push_alignment(ID, N) \
    GCC_BAD("#pragma pack(push[, id], <n>) is not supported on this target")
#define pop_alignment(ID) \
    GCC_BAD("#pragma pack(pop[, id], <n>) is not supported on this target")
#endif /* HANDLE_PRAGMA_PACK_PUSH_POP */

/* #pragma pack ()
   #pragma pack (N)
   
   #pragma pack (push, N)
   #pragma pack (push, ID, N)
   #pragma pack (pop)
   #pragma pack (pop, ID) */
static void
handle_pragma_pack (dummy)
     cpp_reader *dummy ATTRIBUTE_UNUSED;
{
  tree x, id = 0;
  int align = -1;
  enum cpp_ttype token;
  enum { set, push, pop } action;

  if (c_lex (&x) != CPP_OPEN_PAREN)
    GCC_BAD ("missing '(' after '#pragma pack' - ignored");

  token = c_lex (&x);
  if (token == CPP_CLOSE_PAREN)
    {
      action = set;
      align = 0;
    }
  else if (token == CPP_NUMBER)
    {
      align = TREE_INT_CST_LOW (x);
      action = set;
      if (c_lex (&x) != CPP_CLOSE_PAREN)
	GCC_BAD ("malformed '#pragma pack' - ignored");
    }
  else if (token == CPP_NAME)
    {
#define GCC_BAD_ACTION do { if (action == push) \
	  GCC_BAD ("malformed '#pragma pack(push[, id], <n>)' - ignored"); \
	else \
	  GCC_BAD ("malformed '#pragma pack(pop[, id])' - ignored"); \
	} while (0)

      const char *op = IDENTIFIER_POINTER (x);
      if (!strcmp (op, "push"))
	action = push;
      else if (!strcmp (op, "pop"))
	action = pop;
      else
	GCC_BAD2 ("unknown action '%s' for '#pragma pack' - ignored", op);

      token = c_lex (&x);
      if (token != CPP_COMMA && action == push)
	GCC_BAD_ACTION;

      if (token == CPP_COMMA)
	{
	  token = c_lex (&x);
	  if (token == CPP_NAME)
	    {
	      id = x;
	      if (action == push && c_lex (&x) != CPP_COMMA)
		GCC_BAD_ACTION;
	      token = c_lex (&x);
	    }

	  if (action == push)
	    {
	      if (token == CPP_NUMBER)
		{
		  align = TREE_INT_CST_LOW (x);
		  token = c_lex (&x);
		}
	      else
		GCC_BAD_ACTION;
	    }
	}

      if (token != CPP_CLOSE_PAREN)
	GCC_BAD_ACTION;
#undef GCC_BAD_ACTION
    }
  else
    GCC_BAD ("malformed '#pragma pack' - ignored");

  if (c_lex (&x) != CPP_EOF)
    warning ("junk at end of '#pragma pack'");

  if (action != pop)
    switch (align)
      {
      case 0:
      case 1:
      case 2:
      case 4:
      case 8:
      case 16:
	align *= BITS_PER_UNIT;
	break;
      default:
	GCC_BAD2 ("alignment must be a small power of two, not %d", align);
      }

  switch (action)
    {
    case set:   SET_GLOBAL_ALIGNMENT (align);  break;
    case push:  push_alignment (align, id);    break;
    case pop:   pop_alignment (id);            break;
    }
}
#endif  /* HANDLE_PRAGMA_PACK */

#ifdef HANDLE_PRAGMA_WEAK
static void apply_pragma_weak PARAMS ((tree, tree));
static void handle_pragma_weak PARAMS ((cpp_reader *));

static tree pending_weaks;

static void
apply_pragma_weak (decl, value)
     tree decl, value;
{
  if (value)
    decl_attributes (&decl, build_tree_list (get_identifier ("alias"),
				             build_tree_list (NULL, value)),
		     0);
  declare_weak (decl);
}

void
maybe_apply_pragma_weak (decl)
     tree decl;
{
  tree *p, t, id;

  /* Copied from the check in set_decl_assembler_name.  */
  if (TREE_CODE (decl) == FUNCTION_DECL
      || (TREE_CODE (decl) == VAR_DECL 
          && (TREE_STATIC (decl) 
              || DECL_EXTERNAL (decl) 
              || TREE_PUBLIC (decl))))
    id = DECL_ASSEMBLER_NAME (decl);
  else
    return;

  for (p = &pending_weaks; (t = *p) ; p = &TREE_CHAIN (t))
    if (id == TREE_PURPOSE (t))
      {
	apply_pragma_weak (decl, TREE_VALUE (t));
	*p = TREE_CHAIN (t);
	break;
      }
}

/* #pragma weak name [= value] */
static void
handle_pragma_weak (dummy)
     cpp_reader *dummy ATTRIBUTE_UNUSED;
{
  tree name, value, x, decl;
  enum cpp_ttype t;

  value = 0;

  if (c_lex (&name) != CPP_NAME)
    GCC_BAD ("malformed #pragma weak, ignored");
  t = c_lex (&x);
  if (t == CPP_EQ)
    {
      if (c_lex (&value) != CPP_NAME)
	GCC_BAD ("malformed #pragma weak, ignored");
      t = c_lex (&x);
    }
  if (t != CPP_EOF)
    warning ("junk at end of #pragma weak");

  decl = identifier_global_value (name);
  if (decl && TREE_CODE_CLASS (TREE_CODE (decl)) == 'd')
    apply_pragma_weak (decl, value);
  else
    pending_weaks = tree_cons (name, value, pending_weaks);
}
#else
void
maybe_apply_pragma_weak (decl)
     tree decl ATTRIBUTE_UNUSED;
{
}
#endif /* HANDLE_PRAGMA_WEAK */

void
init_pragma ()
{
#ifdef HANDLE_PRAGMA_PACK
  cpp_register_pragma (parse_in, 0, "pack", handle_pragma_pack);
#endif
#ifdef HANDLE_PRAGMA_WEAK
  cpp_register_pragma (parse_in, 0, "weak", handle_pragma_weak);
  ggc_add_tree_root (&pending_weaks, 1);
#endif
#ifdef REGISTER_TARGET_PRAGMAS
  REGISTER_TARGET_PRAGMAS (parse_in);
#endif

#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
  ggc_add_root (&alignment_stack, 1, sizeof(alignment_stack),
		mark_align_stack);
#endif
}
