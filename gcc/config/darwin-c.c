/* Darwin support needed only by C/C++ frontends.
   Copyright (C) 2001
   Free Software Foundation, Inc.
   Contributed by Apple Computer Inc.

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
#include "cpplib.h"
#include "tree.h"
#include "c-pragma.h"
#include "c-lex.h"
#include "c-tree.h"
#include "toplev.h"
#include "tm_p.h"

/* Pragmas.  */

#define BAD(msgid) do { warning (msgid); return; } while (0)

/* Maintain a small stack of alignments.  This is similar to pragma
   pack's stack, but simpler.  */

static void push_field_alignment PARAMS ((int));
static void pop_field_alignment PARAMS ((void));

typedef struct align_stack
{
  int alignment;
  struct align_stack * prev;
} align_stack;

static struct align_stack * field_align_stack = NULL;

static void
push_field_alignment (bit_alignment)
     int bit_alignment;
{
  align_stack *entry = (align_stack *) xmalloc (sizeof (align_stack));

  entry->alignment = maximum_field_alignment;
  entry->prev = field_align_stack;
  field_align_stack = entry;

  maximum_field_alignment = bit_alignment;
}

static void
pop_field_alignment ()
{
  if (field_align_stack)
    {
      align_stack *entry = field_align_stack;

      maximum_field_alignment = entry->alignment;
      field_align_stack = entry->prev;
      free (entry);
    }
  else
    error ("too many #pragma options align=reset");
}

/* Handlers for Darwin-specific pragmas.  */

void
darwin_pragma_ignore (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  /* Do nothing.  */
}

/* #pragma options align={mac68k|power|reset} */

void
darwin_pragma_options (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  char *arg;
  tree t, x;

  if (c_lex (&t) != CPP_NAME)
    BAD ("malformed '#pragma options', ignoring");
  arg = IDENTIFIER_POINTER (t);
  if (strcmp (arg, "align"))
    BAD ("malformed '#pragma options', ignoring");
  if (c_lex (&t) != CPP_EQ)
    BAD ("malformed '#pragma options', ignoring");
  if (c_lex (&t) != CPP_NAME)
    BAD ("malformed '#pragma options', ignoring");

  if (c_lex (&x) != CPP_EOF)
    warning ("junk at end of '#pragma options'");

  arg = IDENTIFIER_POINTER (t);
  if (!strcmp (arg, "mac68k"))
    push_field_alignment (16);
  else if (!strcmp (arg, "power"))
    push_field_alignment (0);
  else if (!strcmp (arg, "reset"))
    pop_field_alignment ();
  else
    warning ("malformed '#pragma options align={mac68k|power|reset}', ignoring");
}

/* #pragma unused ([var {, var}*]) */

void
darwin_pragma_unused (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
  tree decl, x;
  int tok;

  if (c_lex (&x) != CPP_OPEN_PAREN)
    BAD ("missing '(' after '#pragma unused', ignoring");

  while (1)
    {
      tok = c_lex (&decl);
      if (tok == CPP_NAME && decl)
	{
	  tree local = IDENTIFIER_LOCAL_VALUE (decl);
	  if (local && (TREE_CODE (local) == PARM_DECL
			|| TREE_CODE (local) == VAR_DECL))
	    TREE_USED (local) = 1;
	  tok = c_lex (&x);
	  if (tok != CPP_COMMA)
	    break;
	}
    }

  if (tok != CPP_CLOSE_PAREN)
    BAD ("missing ')' after '#pragma unused', ignoring");

  if (c_lex (&x) != CPP_EOF)
    warning ("junk at end of '#pragma unused'");
}
