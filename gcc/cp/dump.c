/* Tree-dumping functionality for intermediate representation.
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
   Written by Mark Mitchell <mark@codesourcery.com>

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
#include "tree.h"
#include "cp-tree.h"
#include "c-dump.h"

static void dump_access
  PARAMS ((dump_info_p, tree));

/* Dump a representation of the accessibility information associated
   with T.  */

static void
dump_access (di, t)
     dump_info_p di;
     tree t;
{
  if (TREE_PROTECTED(t))
    dump_string (di, "protected");
  else if (TREE_PRIVATE(t))
    dump_string (di, "private");
  else
    dump_string (di, "public");
}

int
cp_dump_tree (di, t)
     dump_info_p di;
     tree t;
{
  enum tree_code code;

  /* Figure out what kind of node this is.  */
  code = TREE_CODE (t);

  if (DECL_P (t))
    {
      if (DECL_LANG_SPECIFIC (t) && DECL_LANGUAGE (t) != lang_cplusplus)
	dump_string (di, language_to_string (DECL_LANGUAGE (t), 0));
    }

  switch (code)
    {
    case IDENTIFIER_NODE:
      if (IDENTIFIER_OPNAME_P (t))
	{
	  dump_string (di, "operator");
	  return 1;
	}
      else if (IDENTIFIER_TYPENAME_P (t))
	{
	  dump_child ("tynm", TREE_TYPE (t));
	  return 1;
	}
      else if (t == anonymous_namespace_name)
	{
	  dump_string (di, "unnamed");
	  return 1;
	}
      break;

    case POINTER_TYPE:
      if (TYPE_PTRMEM_P (t))
	{
	  dump_string (di, "ptrmem");
	  dump_child ("ptd", TYPE_PTRMEM_POINTED_TO_TYPE (t));
	  dump_child ("cls", TYPE_PTRMEM_CLASS_TYPE (t));
	  return 1;
	}
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	{
	  dump_string (di, "ptrmem");
	  dump_child ("ptd", TYPE_PTRMEM_POINTED_TO_TYPE (t));
	  dump_child ("cls", TYPE_PTRMEM_CLASS_TYPE (t));
	  return 1;
	}

      dump_child ("vfld", TYPE_VFIELD (t));

      if (!dump_flag (di, TDF_SLIM, t))
	{
	  int i;
	  
	  for (i = 0; i < CLASSTYPE_N_BASECLASSES (t); ++i)
	    {
	      tree base_binfo = BINFO_BASETYPE (TYPE_BINFO (t), i);
	      dump_child ("base", BINFO_TYPE (base_binfo));
	      if (TREE_VIA_VIRTUAL (base_binfo)) 
		dump_string (di, "virtual");
	      dump_access (di, base_binfo);
	    }
	}
      break;

    case FIELD_DECL:
      dump_access (di, t);
      break;

    case FUNCTION_DECL:
      if (!DECL_THUNK_P (t))
	{
	  if (DECL_FUNCTION_MEMBER_P (t)) 
	    {
	      dump_string (di, "member");
	      dump_access (di, t);
	    }
	  if (DECL_CONSTRUCTOR_P (t))
	    dump_string (di, "constructor");
	  if (DECL_DESTRUCTOR_P (t))
	    dump_string (di, "destructor");
	  if (DECL_OVERLOADED_OPERATOR_P (t))
	    dump_string (di, "operator");
	  if (DECL_CONV_FN_P (t))
	    dump_string (di, "conversion");
	  if (DECL_GLOBAL_CTOR_P (t) || DECL_GLOBAL_DTOR_P (t))
	    {
	      if (DECL_GLOBAL_CTOR_P (t))
		dump_string (di, "global init");
	      if (DECL_GLOBAL_DTOR_P (t))
		dump_string (di, "global fini");
	      dump_int (di, "prio", GLOBAL_INIT_PRIORITY (t));
	    }
	  if (DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION (t))
	    dump_string (di, "pseudo tmpl");
	}
      else
	{
	  dump_string (di, "thunk");
	  dump_int (di, "dlta", THUNK_DELTA (t));
	  dump_child ("vcll", THUNK_VCALL_OFFSET (t));
	  dump_child ("fn", DECL_INITIAL (t));
	}
      break;

    case NAMESPACE_DECL:
      /* The fake `::std' namespace does not have DECL_LANG_SPECIFIC,
	 and therefore many other macros do not work on it.  */
      if (t == fake_std_node)
	break;
      if (DECL_NAMESPACE_ALIAS (t))
	dump_child ("alis", DECL_NAMESPACE_ALIAS (t));
      else if (!dump_flag (di, TDF_SLIM, t))
	dump_child ("dcls", cp_namespace_decls (t));
      break;

    case TEMPLATE_DECL:
      dump_child ("rslt", DECL_TEMPLATE_RESULT (t));
      dump_child ("inst", DECL_TEMPLATE_INSTANTIATIONS (t));
      dump_child ("spcs", DECL_TEMPLATE_SPECIALIZATIONS (t));
      dump_child ("prms", DECL_TEMPLATE_PARMS (t));
      break;

    case OVERLOAD:
      dump_child ("crnt", OVL_CURRENT (t));
      dump_child ("chan", OVL_CHAIN (t));
      break;

    case TRY_BLOCK:
      dump_stmt (di, t);
      if (CLEANUP_P (t))
	dump_string (di, "cleanup");
      dump_child ("body", TRY_STMTS (t));
      dump_child ("hdlr", TRY_HANDLERS (t));
      dump_next_stmt (di, t);
      break;

    case EH_SPEC_BLOCK:
      dump_stmt (di, t);
      dump_child ("body", EH_SPEC_STMTS (t));
      dump_child ("raises", EH_SPEC_RAISES (t));
      dump_next_stmt (di, t);
      break;

    case PTRMEM_CST:
      dump_child ("clas", PTRMEM_CST_CLASS (t));
      dump_child ("mbr", PTRMEM_CST_MEMBER (t));
      break;

    case THROW_EXPR:
      /* These nodes are unary, but do not have code class `1'.  */
      dump_child ("op 0", TREE_OPERAND (t, 0));
      break;

    case AGGR_INIT_EXPR:
      dump_int (di, "ctor", AGGR_INIT_VIA_CTOR_P (t));
      dump_child ("fn", TREE_OPERAND (t, 0));
      dump_child ("args", TREE_OPERAND (t, 1));
      dump_child ("decl", TREE_OPERAND (t, 2));
      break;
      
    case CLEANUP_STMT:
      dump_stmt (di, t);
      dump_child ("decl", CLEANUP_DECL (t));
      dump_child ("expr", CLEANUP_EXPR (t));
      dump_next_stmt (di, t);
      break;

    case CTOR_STMT:
      dump_stmt (di, t);
      if (CTOR_BEGIN_P (t))
	dump_string (di, "begn");
      else
	dump_string (di, "end");
      dump_next_stmt (di, t);
      break;

    case HANDLER:
      dump_stmt (di, t);
      dump_child ("parm", HANDLER_PARMS (t));
      dump_child ("body", HANDLER_BODY (t));
      dump_next_stmt (di, t);
      break;

    case MUST_NOT_THROW_EXPR:
      dump_stmt (di, t);
      dump_child ("body", TREE_OPERAND (t, 0));
      dump_next_stmt (di, t);
      break;

    case SUBOBJECT:
      dump_stmt (di, t);
      dump_child ("clnp", TREE_OPERAND (t, 0));
      dump_next_stmt (di, t);
      break;

    case USING_STMT:
      dump_stmt (di, t);
      dump_child ("nmsp", USING_STMT_NAMESPACE (t));
      dump_next_stmt (di, t);
      break;
      
    default:
      break;
    }

  return 0;
}

