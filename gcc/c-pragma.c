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
#include "c-common.h"
#include "output.h"
#include "tm_p.h"

#define GCC_BAD(msgid) do { warning (msgid); return; } while (0)
#define GCC_BAD2(msgid, arg) do { warning (msgid, arg); return; } while (0)

typedef struct align_stack GTY(())
{
  int                  alignment;
  unsigned int         num_pushes;
  tree                 id;
  struct align_stack * prev;
} align_stack;

static GTY(()) struct align_stack * alignment_stack;

#ifdef HANDLE_PRAGMA_PACK
static void handle_pragma_pack PARAMS ((cpp_reader *));

#ifdef HANDLE_PRAGMA_PACK_PUSH_POP
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

      entry = (align_stack *) ggc_alloc (sizeof (* entry));

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

      alignment_stack = entry;
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

static GTY(()) tree pending_weaks;

#ifdef HANDLE_PRAGMA_WEAK
static void apply_pragma_weak PARAMS ((tree, tree));
static void handle_pragma_weak PARAMS ((cpp_reader *));

static void
apply_pragma_weak (decl, value)
     tree decl, value;
{
  if (value)
    {
      value = build_string (IDENTIFIER_LENGTH (value),
			    IDENTIFIER_POINTER (value));
      decl_attributes (&decl, build_tree_list (get_identifier ("alias"),
					       build_tree_list (NULL, value)),
		       0);
    }

  if (SUPPORTS_WEAK && DECL_EXTERNAL (decl) && TREE_USED (decl)
      && TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
    warning_with_decl (decl, "applying #pragma weak `%s' after first use results in unspecified behavior");

  declare_weak (decl);
}

void
maybe_apply_pragma_weak (decl)
     tree decl;
{
  tree *p, t, id;

  /* Avoid asking for DECL_ASSEMBLER_NAME when it's not needed.  */

  /* No weak symbols pending, take the short-cut.  */
  if (!pending_weaks)
    return;
  /* If it's not visible outside this file, it doesn't matter whether
     it's weak.  */
  if (!DECL_EXTERNAL (decl) && !TREE_PUBLIC (decl))
    return;
  /* If it's not a function or a variable, it can't be weak.
     FIXME: what kinds of things are visible outside this file but
     aren't functions or variables?   Should this be an abort() instead?  */
  if (TREE_CODE (decl) != FUNCTION_DECL && TREE_CODE (decl) != VAR_DECL)
    return;

  id = DECL_ASSEMBLER_NAME (decl);

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
    {
      apply_pragma_weak (decl, value);
      if (value)
	assemble_alias (decl, value);
    }
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

static GTY(()) tree pending_redefine_extname;

#ifdef HANDLE_PRAGMA_REDEFINE_EXTNAME
static void handle_pragma_redefine_extname PARAMS ((cpp_reader *));

/* #pragma redefined_extname oldname newname */
static void
handle_pragma_redefine_extname (dummy)
     cpp_reader *dummy ATTRIBUTE_UNUSED;
{
  tree oldname, newname, decl, x;
  enum cpp_ttype t;

  if (c_lex (&oldname) != CPP_NAME)
    {
      warning ("malformed #pragma redefine_extname, ignored");
      return;
    }
  if (c_lex (&newname) != CPP_NAME)
    {
      warning ("malformed #pragma redefine_extname, ignored");
      return;
    }
  t = c_lex (&x);
  if (t != CPP_EOF)
    warning ("junk at end of #pragma redefine_extname");

  decl = identifier_global_value (oldname);
  if (decl && TREE_CODE_CLASS (TREE_CODE (decl)) == 'd')
    {
      if (DECL_ASSEMBLER_NAME_SET_P (decl)
	  && DECL_ASSEMBLER_NAME (decl) != newname)
        warning ("#pragma redefine_extname conflicts with declaration");
      SET_DECL_ASSEMBLER_NAME (decl, newname);
    }
  else
    add_to_renaming_pragma_list(oldname, newname);
}
#endif

void
add_to_renaming_pragma_list (oldname, newname)
	tree oldname, newname;
{
  pending_redefine_extname
    = tree_cons (oldname, newname, pending_redefine_extname);
}

static GTY(()) tree pragma_extern_prefix;

#ifdef HANDLE_PRAGMA_EXTERN_PREFIX
static void handle_pragma_extern_prefix PARAMS ((cpp_reader *));

/* #pragma extern_prefix "prefix" */
static void
handle_pragma_extern_prefix (dummy)
     cpp_reader *dummy ATTRIBUTE_UNUSED;
{
  tree prefix, x;
  enum cpp_ttype t;

  if (c_lex (&prefix) != CPP_STRING)
    {
      warning ("malformed #pragma extern_prefix, ignored");
      return;
    }
  t = c_lex (&x);
  if (t != CPP_EOF)
    warning ("junk at end of #pragma extern_prefix");

  /* Note that the length includes the null terminator.  */
  pragma_extern_prefix = (TREE_STRING_LENGTH (prefix) > 1 ? prefix : NULL);
}
#endif

/* Hook from the front ends to apply the results of one of the preceeding
   pragmas that rename variables.  */

tree
maybe_apply_renaming_pragma (decl, asmname)
     tree decl, asmname;
{
  tree oldname;

  /* Copied from the check in set_decl_assembler_name.  */
  if (TREE_CODE (decl) == FUNCTION_DECL
      || (TREE_CODE (decl) == VAR_DECL 
          && (TREE_STATIC (decl) 
              || DECL_EXTERNAL (decl) 
              || TREE_PUBLIC (decl))))
    oldname = DECL_ASSEMBLER_NAME (decl);
  else
    return asmname;

  /* If the name begins with a *, that's a sign of an asmname attached to
     a previous declaration.  */
  if (IDENTIFIER_POINTER (oldname)[0] == '*')
    {
      const char *oldasmname = IDENTIFIER_POINTER (oldname) + 1;
      if (asmname && strcmp (TREE_STRING_POINTER (asmname), oldasmname) != 0)
	warning ("asm declaration conflicts with previous rename");
      asmname = build_string (strlen (oldasmname), oldasmname);
    }

  {
    tree *p, t;

    for (p = &pending_redefine_extname; (t = *p) ; p = &TREE_CHAIN (t))
      if (oldname == TREE_PURPOSE (t))
	{
	  const char *newname = IDENTIFIER_POINTER (TREE_VALUE (t));

	  if (asmname && strcmp (TREE_STRING_POINTER (asmname), newname) != 0)
            warning ("#pragma redefine_extname conflicts with declaration");
	  *p = TREE_CHAIN (t);

	  return build_string (strlen (newname), newname);
	}
  }

#ifdef HANDLE_PRAGMA_EXTERN_PREFIX
  if (pragma_extern_prefix && !asmname)
    {
      char *x = concat (TREE_STRING_POINTER (pragma_extern_prefix),
			IDENTIFIER_POINTER (oldname), NULL);
      asmname = build_string (strlen (x), x);
      free (x);
      return asmname;
    }
#endif

  return asmname;
}

void
init_pragma ()
{
#ifdef HANDLE_PRAGMA_PACK
  cpp_register_pragma (parse_in, 0, "pack", handle_pragma_pack);
#endif
#ifdef HANDLE_PRAGMA_WEAK
  cpp_register_pragma (parse_in, 0, "weak", handle_pragma_weak);
#endif
#ifdef HANDLE_PRAGMA_REDEFINE_EXTNAME
  cpp_register_pragma (parse_in, 0, "redefine_extname",
		       handle_pragma_redefine_extname);
#endif
#ifdef HANDLE_PRAGMA_EXTERN_PREFIX
  cpp_register_pragma (parse_in, 0, "extern_prefix",
		       handle_pragma_extern_prefix);
#endif

#ifdef REGISTER_TARGET_PRAGMAS
  REGISTER_TARGET_PRAGMAS (parse_in);
#endif
}

#include "gt-c-pragma.h"
