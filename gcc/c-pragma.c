/* Handle #pragma, system V.4 style.  Supports #pragma weak and #pragma pack.
   Copyright (C) 1992, 1997, 1998, 1999 Free Software Foundation, Inc.

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
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "except.h"
#include "function.h"
#include "defaults.h"
#include "c-pragma.h"
#include "flags.h"
#include "toplev.h"

#ifdef HANDLE_GENERIC_PRAGMAS

#ifdef HANDLE_PRAGMA_PACK
/* When structure field packing is in effect, this variable is the
   number of bits to use as the maximum alignment.  When packing is not
   in effect, this is zero.  */

extern int maximum_field_alignment;
#endif


#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
typedef struct align_stack
{
  int                  alignment;
  unsigned int         num_pushes;
  tree                 id;
  struct align_stack * prev;
} align_stack;

static struct align_stack * alignment_stack = NULL;

static int  push_alignment PROTO((int, tree));
static int  pop_alignment  PROTO((tree));

/* Push an alignment value onto the stack.  */
static int
push_alignment (alignment, id)
     int alignment;
     tree id;
{
  switch (alignment)
    {
    case 0:
    case 1:
    case 2:
    case 4:
    case 8:
    case 16:
      break;
    default:
      warning ("\
Alignment must be a small power of two, not %d, in #pragma pack",
	       alignment);
      return 0;
    }
  
  if (alignment_stack == NULL
      || alignment_stack->alignment != alignment
      || id != NULL_TREE)
    {
      align_stack * entry;

      entry = (align_stack *) xmalloc (sizeof (* entry));

      if (entry == NULL)
	{
	  warning ("Out of memory pushing #pragma pack");
	  return 0;
	}

      entry->alignment  = alignment;
      entry->num_pushes = 1;
      entry->id         = id;
      entry->prev       = alignment_stack;
      
      alignment_stack = entry;

      maximum_field_alignment = alignment * 8;
    }
  else
    alignment_stack->num_pushes ++;

  return 1;
}

/* Undo a push of an alignment onto the stack.  */
static int
pop_alignment (id)
     tree id;
{
  align_stack * entry;
      
  if (alignment_stack == NULL)
    {
      warning ("\
#pragma pack (pop) encountered without matching #pragma pack (push, <n>)"
	       );
      return 0;
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
	maximum_field_alignment = 0;
      else
	maximum_field_alignment = entry->alignment * 8;

      free (alignment_stack);

      alignment_stack = entry;
    }

  return 1;
}

/* Generate 'packed' and 'aligned' attributes for decls whilst a
   #pragma pack(push... is in effect.  */
void
insert_pack_attributes (node, attributes, prefix)
     tree node;
     tree * attributes;
     tree * prefix;
{
  tree a;
  int field_alignment;

  /* If we are not packing, then there is nothing to do.  */
  if (maximum_field_alignment == 0
      || alignment_stack == NULL)
    return;

  /* We are only interested in fields.  */
  if (TREE_CODE_CLASS (TREE_CODE (node)) != 'd'
      || TREE_CODE (node) != FIELD_DECL)
    return;
  
  field_alignment = TYPE_ALIGN (TREE_TYPE (node));
  if (field_alignment <= 0 || field_alignment > maximum_field_alignment)
    field_alignment = maximum_field_alignment;

  /* Add a 'packed' attribute.  */
  * attributes = tree_cons (get_identifier ("packed"), NULL, * attributes);
  
  /* If the alignment is > 8 then add an alignment attribute as well.  */
  if (field_alignment > 8)
    {
      /* If the aligned attribute is already present then do not override it.  */
      for (a = * attributes; a; a = TREE_CHAIN (a))
	{
	  tree name = TREE_PURPOSE (a);
	  if (strcmp (IDENTIFIER_POINTER (name), "aligned") == 0)
	    break;
	}
      
      if (a == NULL)
	for (a = * prefix; a; a = TREE_CHAIN (a))
	  {
	    tree name = TREE_PURPOSE (a);
	    if (strcmp (IDENTIFIER_POINTER (name), "aligned") == 0)
	      break;
	  }
  
      if (a == NULL)
	{
	  * attributes = tree_cons
	      (get_identifier ("aligned"),
	       tree_cons (NULL,
			  build_int_2 (field_alignment / 8, 0),
			  NULL),
	       * attributes);
	}
    }

  return;
}
#endif /* HANDLE_PRAGMA_PACK_PUSH_POP */

/* Handle one token of a pragma directive.  TOKEN is the current token, and
   STRING is its printable form.  Some front ends do not support generating
   tokens, and will only pass in a STRING.  Also some front ends will reuse
   the buffer containing STRING, so it must be copied to a local buffer if
   it needs to be preserved.

   If STRING is non-NULL, then the return value will be ignored, and there
   will be futher calls to handle_pragma_token() in order to handle the rest of
   the line containing the #pragma directive.  If STRING is NULL, the entire
   line has now been presented to handle_pragma_token() and the return value
   should be zero if the pragma flawed in some way, or if the pragma was not
   recognised, and non-zero if it was successfully handled.  */

int
handle_pragma_token (string, token)
     const char * string;
     tree token;
{
  static enum pragma_state state = ps_start;
  static enum pragma_state type;
  static char * name;
  static char * value;
  static int align;
  static tree id;

  /* If we have reached the end of the #pragma directive then
     determine what value we should return.  */
  
  if (string == NULL)
    {
      int ret_val = 0;

      switch (type)
	{
	default:
	  abort ();
	  break;

	case ps_done:
	  /* The pragma was not recognised.  */
	  break;
	  
#ifdef HANDLE_PRAGMA_PACK	  
	case ps_pack:
	  if (state == ps_right)
	    {
	      maximum_field_alignment = align * 8;
	      ret_val = 1;
	    }
	  else
	    warning ("malformed `#pragma pack'");
	  break;
#endif /* HANDLE_PRAGMA_PACK */
	  
#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
	case ps_push:
	  if (state == ps_right)
	    ret_val = push_alignment (align, id);
	  else
	    warning ("malformed '#pragma pack(push[,id],<n>)'");
	  break;
	  
	case ps_pop:
	  if (state == ps_right)
	    ret_val = pop_alignment (id);
	  else
	    warning ("malformed '#pragma pack(pop[,id])'");
	  break;
#endif /* HANDLE_PRAGMA_PACK_PUSH_POP */
	  
#ifdef HANDLE_PRAGMA_WEAK
	case ps_weak:
	  if (HANDLE_PRAGMA_WEAK)
	    {
	      if (state == ps_name)
		ret_val = add_weak (name, NULL);
	      else if (state == ps_value)
		ret_val = add_weak (name, value);
	      else
		warning ("malformed `#pragma weak'");
	    }
	  else
	    ret_val = 1; /* Ignore the pragma.  */
	  break;
#endif /* HANDLE_PRAGMA_WEAK */
	}

      type = state = ps_start;
      id = NULL_TREE;
      
      return ret_val;
    }

  /* If we have been given a token, but it is not an identifier,
     or a small constant, then something has gone wrong.  */
  if (token)
    {
      switch (TREE_CODE (token))
	{
	case IDENTIFIER_NODE:
	  break;
	  
	case INTEGER_CST:
	  if (TREE_INT_CST_HIGH (token) != 0)
	    return 0;
	  break;
	  
	default:
	  return 0;
	}
    }
      
  switch (state)
    {
    case ps_start:
      type = state = ps_done;
#ifdef HANDLE_PRAGMA_PACK
      if (strcmp (string, "pack") == 0)
	type = state = ps_pack;
#endif
#ifdef HANDLE_PRAGMA_WEAK
      if (strcmp (string, "weak") == 0)
	type = state = ps_weak;
#endif	  
      break;
      
#ifdef HANDLE_PRAGMA_WEAK
    case ps_weak:
      name = permalloc (strlen (string) + 1);
      if (name == NULL)
	{
	  warning ("Out of memory parsing #pragma weak");
	  state = ps_bad;
	}
      else
	{
	  strcpy (name, string);
	  state = ps_name;
	}
      break;
      
    case ps_name:
      state = (strcmp (string, "=") ? ps_bad : ps_equals);
      break;

    case ps_equals:
      value = permalloc (strlen (string) + 1);
      if (value == NULL)
	{
	  warning ("Out of memory parsing #pragma weak");
	  state = ps_bad;
	}
      else
	{
	  strcpy (value, string);
	  state = ps_value;
	}
      break;

    case ps_value:
      state = ps_bad;
      break;
#endif /* HANDLE_PRAGMA_WEAK */
      
#ifdef HANDLE_PRAGMA_PACK
    case ps_pack:
      state = (strcmp (string, "(") ? ps_bad : ps_left);
      break;

    case ps_left:

      if (token == NULL_TREE)
	{
	  /* #pragma pack () resets packing rules to their
	     defaults.  */
	  if (strcmp (string, ")") == 0)
	    {
	      align = 0;
	      state = ps_right;
	    }
	  else
	    state = ps_bad;
	}
      else if (TREE_CODE (token) == INTEGER_CST)
	goto handle_align;

#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
      else if (TREE_CODE (token) == IDENTIFIER_NODE)
	{
	  if (strcmp (string, "push") == 0)
	    type = state = ps_push;
	  else if (strcmp (string, "pop") == 0)
	    type = state = ps_pop;
	  else
	    state = ps_bad;
	}
#endif
      else
	state = ps_bad;
      break;

    handle_align:
      align = TREE_INT_CST_LOW (token);
      switch (align)
	{
	case 1:
	case 2:
	case 4:
	case 8:
	case 16:
	  state = ps_align;
	  break;

	default:
	  state = ps_bad;
	  break;
	}
      break;

    case ps_align:
      state = (strcmp (string, ")") ? ps_bad : ps_right);
      break;

    case ps_right:
      state = ps_bad;
      break;
#endif /* HANDLE_PRAGMA_PACK */

#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
    case ps_push:
      state = (strcmp (string, ",") ? ps_bad : ps_pushcomma);
      break;

    case ps_pushid:
      state = (strcmp (string, ",") ? ps_bad : ps_pushcomma2);
      break;

    case ps_pushcomma:
      if (token && TREE_CODE (token) == IDENTIFIER_NODE)
	{
	  id = token;
	  state = ps_pushid;
	  break;
	}

      /* else fall through */
    case ps_pushcomma2:
      if (token && TREE_CODE (token) == INTEGER_CST)
	goto handle_align;
      else
	state = ps_bad;
      break;

    case ps_pop:
      if (strcmp (string, ",") == 0)
	state = ps_popcomma;
      else
	state = (strcmp (string, ")") ? ps_bad : ps_right);
      break;

    case ps_popcomma:
      if (token && TREE_CODE (token) == IDENTIFIER_NODE)
	{
	  id = token;
	  state = ps_align;
	}
      else
	state = ps_bad;
      break;
#endif /* HANDLE_PRAGMA_PACK_PUSH_POP */
      
    case ps_bad:
    case ps_done:
      break;

    default:
      abort ();
    }

  return 1;
}
#endif /* HANDLE_GENERIC_PRAGMAS */
