/* Tree-dumping functionality for C-family languages.
   Copyright (C) 2002 Free Software Foundation, Inc.
   Written by Mark Mitchell <mark@codesourcery.com>

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
#include "tree.h"
#include "c-tree.h"
#include "tree-dump.h"

/* Dump information common to statements from STMT.  */

void
dump_stmt (di, t)
     dump_info_p di;
     tree t;
{
  dump_int (di, "line", STMT_LINENO (t));
}

/* Dump the next statement after STMT.  */

void
dump_next_stmt (di, t)
     dump_info_p di;
     tree t;
{
  dump_child ("next", TREE_CHAIN (t));
}

/* Dump any C-specific tree codes and attributes of common codes.  */

int
c_dump_tree (dump_info, t)
     void *dump_info;
     tree t;
{
  enum tree_code code;
  dump_info_p di = (dump_info_p) dump_info;

  /* Figure out what kind of node this is.  */
  code = TREE_CODE (t);

  switch (code)
    {
    case FIELD_DECL:
      if (DECL_C_BIT_FIELD (t))
	dump_string (di, "bitfield");
      break;

    case ASM_STMT:
      dump_stmt (di, t);
      if (ASM_VOLATILE_P (t))
	dump_string (di, "volatile");
      dump_child ("strg", ASM_STRING (t));
      dump_child ("outs", ASM_OUTPUTS (t));
      dump_child ("ins", ASM_INPUTS (t));
      dump_child ("clbr", ASM_CLOBBERS (t));
      dump_next_stmt (di, t);
      break;

    case BREAK_STMT:
    case CONTINUE_STMT:
      dump_stmt (di, t);
      dump_next_stmt (di, t);
      break;

    case CASE_LABEL:
      /* Note that a case label is not like other statements; there is
	 no way to get the line-number of a case label.  */
      dump_child ("low", CASE_LOW (t));
      dump_child ("high", CASE_HIGH (t));
      dump_next_stmt (di, t);
      break;

    case CLEANUP_STMT:
      dump_stmt (di, t);
      dump_child ("decl", CLEANUP_DECL (t));
      dump_child ("expr", CLEANUP_EXPR (t));
      dump_next_stmt (di, t);
      break;

    case COMPOUND_STMT:
      dump_stmt (di, t);
      dump_child ("body", COMPOUND_BODY (t));
      dump_next_stmt (di, t);
      break;

    case DECL_STMT:
      dump_stmt (di, t);
      dump_child ("decl", DECL_STMT_DECL (t));
      dump_next_stmt (di, t);
      break;

    case DO_STMT:
      dump_stmt (di, t);
      dump_child ("body", DO_BODY (t));
      dump_child ("cond", DO_COND (t));
      dump_next_stmt (di, t);
      break;

    case EXPR_STMT:
      dump_stmt (di, t);
      dump_child ("expr", EXPR_STMT_EXPR (t));
      dump_next_stmt (di, t);
      break;

    case FOR_STMT:
      dump_stmt (di, t);
      dump_child ("init", FOR_INIT_STMT (t));
      dump_child ("cond", FOR_COND (t));
      dump_child ("expr", FOR_EXPR (t));
      dump_child ("body", FOR_BODY (t));
      dump_next_stmt (di, t);
      break;

    case GOTO_STMT:
      dump_stmt (di, t);
      dump_child ("dest", GOTO_DESTINATION (t));
      dump_next_stmt (di, t);
      break;

    case IF_STMT:
      dump_stmt (di, t);
      dump_child ("cond", IF_COND (t));
      dump_child ("then", THEN_CLAUSE (t));
      dump_child ("else", ELSE_CLAUSE (t));
      dump_next_stmt (di, t);
      break;

    case LABEL_STMT:
      dump_stmt (di, t);
      dump_child ("labl", LABEL_STMT_LABEL (t));
      dump_next_stmt (di, t);
      break;

    case RETURN_STMT:
      dump_stmt (di, t);
      dump_child ("expr", RETURN_STMT_EXPR (t));
      dump_next_stmt (di, t);
      break;

    case SWITCH_STMT:
      dump_stmt (di, t);
      dump_child ("cond", SWITCH_COND (t));
      dump_child ("body", SWITCH_BODY (t));
      dump_next_stmt (di, t);
      break;

    case WHILE_STMT:
      dump_stmt (di, t);
      dump_child ("cond", WHILE_COND (t));
      dump_child ("body", WHILE_BODY (t));
      dump_next_stmt (di, t);
      break;

    case SCOPE_STMT:
      dump_stmt (di, t);
      if (SCOPE_BEGIN_P (t))
	dump_string (di, "begn");
      else
	dump_string (di, "end");
      if (SCOPE_NULLIFIED_P (t))
	dump_string (di, "null");
      if (!SCOPE_NO_CLEANUPS_P (t))
	dump_string (di, "clnp");
      dump_next_stmt (di, t);
      break;

    case STMT_EXPR:
      dump_child ("stmt", STMT_EXPR_STMT (t));
      break;

    default:
      break;
    }

  return 0;
}
