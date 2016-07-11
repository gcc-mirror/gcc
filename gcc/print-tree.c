/* Prints out tree in human readable form - GCC
   Copyright (C) 1990-2016 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "varasm.h"
#include "print-rtl.h"
#include "stor-layout.h"
#include "langhooks.h"
#include "tree-iterator.h"
#include "gimple-pretty-print.h" /* FIXME */
#include "tree-cfg.h"
#include "tree-dump.h"

/* Define the hash table of nodes already seen.
   Such nodes are not repeated; brief cross-references are used.  */

#define HASH_SIZE 37

struct bucket
{
  tree node;
  struct bucket *next;
};

static struct bucket **table;

/* Print PREFIX and ADDR to FILE.  */
void
dump_addr (FILE *file, const char *prefix, const void *addr)
{
  if (flag_dump_noaddr || flag_dump_unnumbered)
    fprintf (file, "%s#", prefix);
  else
    fprintf (file, "%s" HOST_PTR_PRINTF, prefix, addr);
}

/* Print a node in brief fashion, with just the code, address and name.  */

void
print_node_brief (FILE *file, const char *prefix, const_tree node, int indent)
{
  enum tree_code_class tclass;

  if (node == 0)
    return;

  tclass = TREE_CODE_CLASS (TREE_CODE (node));

  /* Always print the slot this node is in, and its code, address and
     name if any.  */
  if (indent > 0)
    fprintf (file, " ");
  fprintf (file, "%s <%s", prefix, get_tree_code_name (TREE_CODE (node)));
  dump_addr (file, " ", node);

  if (tclass == tcc_declaration)
    {
      if (DECL_NAME (node))
	fprintf (file, " %s", IDENTIFIER_POINTER (DECL_NAME (node)));
      else if (TREE_CODE (node) == LABEL_DECL
	       && LABEL_DECL_UID (node) != -1)
	{
	  if (dump_flags & TDF_NOUID)
	    fprintf (file, " L.xxxx");
	  else
	    fprintf (file, " L.%d", (int) LABEL_DECL_UID (node));
	}
      else
	{
	  if (dump_flags & TDF_NOUID)
	    fprintf (file, " %c.xxxx",
		     TREE_CODE (node) == CONST_DECL ? 'C' : 'D');
	  else
	    fprintf (file, " %c.%u",
		     TREE_CODE (node) == CONST_DECL ? 'C' : 'D',
		     DECL_UID (node));
	}
    }
  else if (tclass == tcc_type)
    {
      if (TYPE_NAME (node))
	{
	  if (TREE_CODE (TYPE_NAME (node)) == IDENTIFIER_NODE)
	    fprintf (file, " %s", IDENTIFIER_POINTER (TYPE_NAME (node)));
	  else if (TREE_CODE (TYPE_NAME (node)) == TYPE_DECL
		   && DECL_NAME (TYPE_NAME (node)))
	    fprintf (file, " %s",
		     IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (node))));
	}
      if (!ADDR_SPACE_GENERIC_P (TYPE_ADDR_SPACE (node)))
	fprintf (file, " address-space-%d", TYPE_ADDR_SPACE (node));
    }
  if (TREE_CODE (node) == IDENTIFIER_NODE)
    fprintf (file, " %s", IDENTIFIER_POINTER (node));

  /* We might as well always print the value of an integer or real.  */
  if (TREE_CODE (node) == INTEGER_CST)
    {
      if (TREE_OVERFLOW (node))
	fprintf (file, " overflow");

      fprintf (file, " ");
      print_dec (node, file, TYPE_SIGN (TREE_TYPE (node)));
    }
  if (TREE_CODE (node) == REAL_CST)
    {
      REAL_VALUE_TYPE d;

      if (TREE_OVERFLOW (node))
	fprintf (file, " overflow");

      d = TREE_REAL_CST (node);
      if (REAL_VALUE_ISINF (d))
	fprintf (file,  REAL_VALUE_NEGATIVE (d) ? " -Inf" : " Inf");
      else if (REAL_VALUE_ISNAN (d))
	fprintf (file, " Nan");
      else
	{
	  char string[60];
	  real_to_decimal (string, &d, sizeof (string), 0, 1);
	  fprintf (file, " %s", string);
	}
    }
  if (TREE_CODE (node) == FIXED_CST)
    {
      FIXED_VALUE_TYPE f;
      char string[60];

      if (TREE_OVERFLOW (node))
	fprintf (file, " overflow");

      f = TREE_FIXED_CST (node);
      fixed_to_decimal (string, &f, sizeof (string));
      fprintf (file, " %s", string);
    }

  fprintf (file, ">");
}

void
indent_to (FILE *file, int column)
{
  int i;

  /* Since this is the long way, indent to desired column.  */
  if (column > 0)
    fprintf (file, "\n");
  for (i = 0; i < column; i++)
    fprintf (file, " ");
}

/* Print the node NODE in full on file FILE, preceded by PREFIX,
   starting in column INDENT.  */

void
print_node (FILE *file, const char *prefix, tree node, int indent)
{
  int hash;
  struct bucket *b;
  machine_mode mode;
  enum tree_code_class tclass;
  int len;
  int i;
  expanded_location xloc;
  enum tree_code code;

  if (node == 0)
    return;

  code = TREE_CODE (node);
  tclass = TREE_CODE_CLASS (code);

  /* Don't get too deep in nesting.  If the user wants to see deeper,
     it is easy to use the address of a lowest-level node
     as an argument in another call to debug_tree.  */

  if (indent > 24)
    {
      print_node_brief (file, prefix, node, indent);
      return;
    }

  if (indent > 8 && (tclass == tcc_type || tclass == tcc_declaration))
    {
      print_node_brief (file, prefix, node, indent);
      return;
    }

  /* It is unsafe to look at any other fields of an ERROR_MARK node.  */
  if (code == ERROR_MARK)
    {
      print_node_brief (file, prefix, node, indent);
      return;
    }

  /* Allow this function to be called if the table is not there.  */
  if (table)
    {
      hash = ((uintptr_t) node) % HASH_SIZE;

      /* If node is in the table, just mention its address.  */
      for (b = table[hash]; b; b = b->next)
	if (b->node == node)
	  {
	    print_node_brief (file, prefix, node, indent);
	    return;
	  }

      /* Add this node to the table.  */
      b = XNEW (struct bucket);
      b->node = node;
      b->next = table[hash];
      table[hash] = b;
    }

  /* Indent to the specified column, since this is the long form.  */
  indent_to (file, indent);

  /* Print the slot this node is in, and its code, and address.  */
  fprintf (file, "%s <%s", prefix, get_tree_code_name (code));
  dump_addr (file, " ", node);

  /* Print the name, if any.  */
  if (tclass == tcc_declaration)
    {
      if (DECL_NAME (node))
	fprintf (file, " %s", IDENTIFIER_POINTER (DECL_NAME (node)));
      else if (code == LABEL_DECL
	       && LABEL_DECL_UID (node) != -1)
	{
	  if (dump_flags & TDF_NOUID)
	    fprintf (file, " L.xxxx");
	  else
	    fprintf (file, " L.%d", (int) LABEL_DECL_UID (node));
	}
      else
	{
	  if (dump_flags & TDF_NOUID)
	    fprintf (file, " %c.xxxx", code == CONST_DECL ? 'C' : 'D');
	  else
	    fprintf (file, " %c.%u", code == CONST_DECL ? 'C' : 'D',
		     DECL_UID (node));
	}
    }
  else if (tclass == tcc_type)
    {
      if (TYPE_NAME (node))
	{
	  if (TREE_CODE (TYPE_NAME (node)) == IDENTIFIER_NODE)
	    fprintf (file, " %s", IDENTIFIER_POINTER (TYPE_NAME (node)));
	  else if (TREE_CODE (TYPE_NAME (node)) == TYPE_DECL
		   && DECL_NAME (TYPE_NAME (node)))
	    fprintf (file, " %s",
		     IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (node))));
	}
    }
  if (code == IDENTIFIER_NODE)
    fprintf (file, " %s", IDENTIFIER_POINTER (node));

  if (code == INTEGER_CST)
    {
      if (indent <= 4)
	print_node_brief (file, "type", TREE_TYPE (node), indent + 4);
    }
  else if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    {
      print_node (file, "type", TREE_TYPE (node), indent + 4);
      if (TREE_TYPE (node))
	indent_to (file, indent + 3);
    }

  if (!TYPE_P (node) && TREE_SIDE_EFFECTS (node))
    fputs (" side-effects", file);

  if (TYPE_P (node) ? TYPE_READONLY (node) : TREE_READONLY (node))
    fputs (" readonly", file);
  if (TYPE_P (node) && TYPE_ATOMIC (node))
    fputs (" atomic", file);
  if (!TYPE_P (node) && TREE_CONSTANT (node))
    fputs (" constant", file);
  else if (TYPE_P (node) && TYPE_SIZES_GIMPLIFIED (node))
    fputs (" sizes-gimplified", file);

  if (TYPE_P (node) && !ADDR_SPACE_GENERIC_P (TYPE_ADDR_SPACE (node)))
    fprintf (file, " address-space-%d", TYPE_ADDR_SPACE (node));

  if (TREE_ADDRESSABLE (node))
    fputs (" addressable", file);
  if (TREE_THIS_VOLATILE (node))
    fputs (" volatile", file);
  if (TREE_ASM_WRITTEN (node))
    fputs (" asm_written", file);
  if (TREE_USED (node))
    fputs (" used", file);
  if (TREE_NOTHROW (node))
    fputs (" nothrow", file);
  if (TREE_PUBLIC (node))
    fputs (" public", file);
  if (TREE_PRIVATE (node))
    fputs (" private", file);
  if (TREE_PROTECTED (node))
    fputs (" protected", file);
  if (TREE_STATIC (node))
    fputs (code == CALL_EXPR ? " must-tail-call" : " static", file);
  if (TREE_DEPRECATED (node))
    fputs (" deprecated", file);
  if (TREE_VISITED (node))
    fputs (" visited", file);

  if (code != TREE_VEC && code != INTEGER_CST && code != SSA_NAME)
    {
      if (TREE_LANG_FLAG_0 (node))
	fputs (" tree_0", file);
      if (TREE_LANG_FLAG_1 (node))
	fputs (" tree_1", file);
      if (TREE_LANG_FLAG_2 (node))
	fputs (" tree_2", file);
      if (TREE_LANG_FLAG_3 (node))
	fputs (" tree_3", file);
      if (TREE_LANG_FLAG_4 (node))
	fputs (" tree_4", file);
      if (TREE_LANG_FLAG_5 (node))
	fputs (" tree_5", file);
      if (TREE_LANG_FLAG_6 (node))
	fputs (" tree_6", file);
    }

  /* DECL_ nodes have additional attributes.  */

  switch (TREE_CODE_CLASS (code))
    {
    case tcc_declaration:
      if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
	{
	  if (DECL_UNSIGNED (node))
	    fputs (" unsigned", file);
	  if (DECL_IGNORED_P (node))
	    fputs (" ignored", file);
	  if (DECL_ABSTRACT_P (node))
	    fputs (" abstract", file);
	  if (DECL_EXTERNAL (node))
	    fputs (" external", file);
	  if (DECL_NONLOCAL (node))
	    fputs (" nonlocal", file);
	}
      if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
	{
	  if (DECL_WEAK (node))
	    fputs (" weak", file);
	  if (DECL_IN_SYSTEM_HEADER (node))
	    fputs (" in_system_header", file);
	}
      if (CODE_CONTAINS_STRUCT (code, TS_DECL_WRTL)
	  && code != LABEL_DECL
	  && code != FUNCTION_DECL
	  && DECL_REGISTER (node))
	fputs (" regdecl", file);

      if (code == TYPE_DECL && TYPE_DECL_SUPPRESS_DEBUG (node))
	fputs (" suppress-debug", file);

      if (code == FUNCTION_DECL
	  && DECL_FUNCTION_SPECIFIC_TARGET (node))
	fputs (" function-specific-target", file);
      if (code == FUNCTION_DECL
	  && DECL_FUNCTION_SPECIFIC_OPTIMIZATION (node))
	fputs (" function-specific-opt", file);
      if (code == FUNCTION_DECL && DECL_DECLARED_INLINE_P (node))
	fputs (" autoinline", file);
      if (code == FUNCTION_DECL && DECL_BUILT_IN (node))
	fputs (" built-in", file);
      if (code == FUNCTION_DECL && DECL_STATIC_CHAIN (node))
	fputs (" static-chain", file);
      if (TREE_CODE (node) == FUNCTION_DECL && decl_is_tm_clone (node))
	fputs (" tm-clone", file);

      if (code == FIELD_DECL && DECL_PACKED (node))
	fputs (" packed", file);
      if (code == FIELD_DECL && DECL_BIT_FIELD (node))
	fputs (" bit-field", file);
      if (code == FIELD_DECL && DECL_NONADDRESSABLE_P (node))
	fputs (" nonaddressable", file);

      if (code == LABEL_DECL && EH_LANDING_PAD_NR (node))
	fprintf (file, " landing-pad:%d", EH_LANDING_PAD_NR (node));

      if (code == VAR_DECL && DECL_IN_TEXT_SECTION (node))
	fputs (" in-text-section", file);
      if (code == VAR_DECL && DECL_IN_CONSTANT_POOL (node))
	fputs (" in-constant-pool", file);
      if (code == VAR_DECL && DECL_COMMON (node))
	fputs (" common", file);
      if (code == VAR_DECL && DECL_THREAD_LOCAL_P (node))
	{
	  fputs (" ", file);
	  fputs (tls_model_names[DECL_TLS_MODEL (node)], file);
	}

      if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
	{
	  if (DECL_VIRTUAL_P (node))
	    fputs (" virtual", file);
	  if (DECL_PRESERVE_P (node))
	    fputs (" preserve", file);
	  if (DECL_LANG_FLAG_0 (node))
	    fputs (" decl_0", file);
	  if (DECL_LANG_FLAG_1 (node))
	    fputs (" decl_1", file);
	  if (DECL_LANG_FLAG_2 (node))
	    fputs (" decl_2", file);
	  if (DECL_LANG_FLAG_3 (node))
	    fputs (" decl_3", file);
	  if (DECL_LANG_FLAG_4 (node))
	    fputs (" decl_4", file);
	  if (DECL_LANG_FLAG_5 (node))
	    fputs (" decl_5", file);
	  if (DECL_LANG_FLAG_6 (node))
	    fputs (" decl_6", file);
	  if (DECL_LANG_FLAG_7 (node))
	    fputs (" decl_7", file);

	  mode = DECL_MODE (node);
	  fprintf (file, " %s", GET_MODE_NAME (mode));
	}

      if ((code == VAR_DECL || code == PARM_DECL || code == RESULT_DECL)
	  && DECL_BY_REFERENCE (node))
	fputs (" passed-by-reference", file);

      if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS)  && DECL_DEFER_OUTPUT (node))
	fputs (" defer-output", file);


      xloc = expand_location (DECL_SOURCE_LOCATION (node));
      fprintf (file, " file %s line %d col %d", xloc.file, xloc.line,
	       xloc.column);

      if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
	{
	  print_node (file, "size", DECL_SIZE (node), indent + 4);
	  print_node (file, "unit size", DECL_SIZE_UNIT (node), indent + 4);

	  if (code != FUNCTION_DECL || DECL_BUILT_IN (node))
	    indent_to (file, indent + 3);

	  if (DECL_USER_ALIGN (node))
	    fprintf (file, " user");

	  fprintf (file, " align %d", DECL_ALIGN (node));
	  if (code == FIELD_DECL)
	    fprintf (file, " offset_align " HOST_WIDE_INT_PRINT_UNSIGNED,
		     DECL_OFFSET_ALIGN (node));

	  if (code == FUNCTION_DECL && DECL_BUILT_IN (node))
	    {
	      if (DECL_BUILT_IN_CLASS (node) == BUILT_IN_MD)
		fprintf (file, " built-in BUILT_IN_MD %d", DECL_FUNCTION_CODE (node));
	      else
		fprintf (file, " built-in %s:%s",
			 built_in_class_names[(int) DECL_BUILT_IN_CLASS (node)],
			 built_in_names[(int) DECL_FUNCTION_CODE (node)]);
	    }
	}
      if (code == FIELD_DECL)
	{
	  print_node (file, "offset", DECL_FIELD_OFFSET (node), indent + 4);
	  print_node (file, "bit offset", DECL_FIELD_BIT_OFFSET (node),
		      indent + 4);
	  if (DECL_BIT_FIELD_TYPE (node))
	    print_node (file, "bit_field_type", DECL_BIT_FIELD_TYPE (node),
			indent + 4);
	}

      print_node_brief (file, "context", DECL_CONTEXT (node), indent + 4);

      if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
	{
	  print_node_brief (file, "attributes",
			    DECL_ATTRIBUTES (node), indent + 4);
	  if (code != PARM_DECL)
	    print_node_brief (file, "initial", DECL_INITIAL (node),
			      indent + 4);
	}
      if (CODE_CONTAINS_STRUCT (code, TS_DECL_WRTL))
	{
	  print_node_brief (file, "abstract_origin",
			    DECL_ABSTRACT_ORIGIN (node), indent + 4);
	}
      if (CODE_CONTAINS_STRUCT (code, TS_DECL_NON_COMMON))
	{
	  print_node (file, "result", DECL_RESULT_FLD (node), indent + 4);
	}

      lang_hooks.print_decl (file, node, indent);

      if (DECL_RTL_SET_P (node))
	{
	  indent_to (file, indent + 4);
	  print_rtl (file, DECL_RTL (node));
	}

      if (code == PARM_DECL)
	{
	  print_node (file, "arg-type", DECL_ARG_TYPE (node), indent + 4);

	  if (DECL_INCOMING_RTL (node) != 0)
	    {
	      indent_to (file, indent + 4);
	      fprintf (file, "incoming-rtl ");
	      print_rtl (file, DECL_INCOMING_RTL (node));
	    }
	}
      else if (code == FUNCTION_DECL
	       && DECL_STRUCT_FUNCTION (node) != 0)
	{
	  print_node (file, "arguments", DECL_ARGUMENTS (node), indent + 4);
	  indent_to (file, indent + 4);
	  dump_addr (file, "struct-function ", DECL_STRUCT_FUNCTION (node));
	}

      if ((code == VAR_DECL || code == PARM_DECL)
	  && DECL_HAS_VALUE_EXPR_P (node))
	print_node (file, "value-expr", DECL_VALUE_EXPR (node), indent + 4);

      /* Print the decl chain only if decl is at second level.  */
      if (indent == 4)
	print_node (file, "chain", TREE_CHAIN (node), indent + 4);
      else
	print_node_brief (file, "chain", TREE_CHAIN (node), indent + 4);
      break;

    case tcc_type:
      if (TYPE_UNSIGNED (node))
	fputs (" unsigned", file);

      if (TYPE_NO_FORCE_BLK (node))
	fputs (" no-force-blk", file);

      if (TYPE_STRING_FLAG (node))
	fputs (" string-flag", file);

      if (TYPE_NEEDS_CONSTRUCTING (node))
	fputs (" needs-constructing", file);

      if ((code == RECORD_TYPE
	   || code == UNION_TYPE
	   || code == QUAL_UNION_TYPE
	   || code == ARRAY_TYPE)
	  && TYPE_REVERSE_STORAGE_ORDER (node))
	fputs (" reverse-storage-order", file);

      /* The transparent-union flag is used for different things in
	 different nodes.  */
      if ((code == UNION_TYPE || code == RECORD_TYPE)
	  && TYPE_TRANSPARENT_AGGR (node))
	fputs (" transparent-aggr", file);
      else if (code == ARRAY_TYPE
	       && TYPE_NONALIASED_COMPONENT (node))
	fputs (" nonaliased-component", file);

      if (TYPE_PACKED (node))
	fputs (" packed", file);

      if (TYPE_RESTRICT (node))
	fputs (" restrict", file);

      if (TYPE_LANG_FLAG_0 (node))
	fputs (" type_0", file);
      if (TYPE_LANG_FLAG_1 (node))
	fputs (" type_1", file);
      if (TYPE_LANG_FLAG_2 (node))
	fputs (" type_2", file);
      if (TYPE_LANG_FLAG_3 (node))
	fputs (" type_3", file);
      if (TYPE_LANG_FLAG_4 (node))
	fputs (" type_4", file);
      if (TYPE_LANG_FLAG_5 (node))
	fputs (" type_5", file);
      if (TYPE_LANG_FLAG_6 (node))
	fputs (" type_6", file);
      if (TYPE_LANG_FLAG_7 (node))
	fputs (" type_7", file);

      mode = TYPE_MODE (node);
      fprintf (file, " %s", GET_MODE_NAME (mode));

      print_node (file, "size", TYPE_SIZE (node), indent + 4);
      print_node (file, "unit size", TYPE_SIZE_UNIT (node), indent + 4);
      indent_to (file, indent + 3);

      if (TYPE_USER_ALIGN (node))
	fprintf (file, " user");

      fprintf (file, " align %d symtab %d alias set " HOST_WIDE_INT_PRINT_DEC,
	       TYPE_ALIGN (node), TYPE_SYMTAB_ADDRESS (node),
	       (HOST_WIDE_INT) TYPE_ALIAS_SET (node));

      if (TYPE_STRUCTURAL_EQUALITY_P (node))
	fprintf (file, " structural equality");
      else
	dump_addr (file, " canonical type ", TYPE_CANONICAL (node));

      print_node (file, "attributes", TYPE_ATTRIBUTES (node), indent + 4);

      if (INTEGRAL_TYPE_P (node) || code == REAL_TYPE
	  || code == FIXED_POINT_TYPE)
	{
	  fprintf (file, " precision %d", TYPE_PRECISION (node));
	  print_node_brief (file, "min", TYPE_MIN_VALUE (node), indent + 4);
	  print_node_brief (file, "max", TYPE_MAX_VALUE (node), indent + 4);
	}

      if (code == ENUMERAL_TYPE)
	print_node (file, "values", TYPE_VALUES (node), indent + 4);
      else if (code == ARRAY_TYPE)
	print_node (file, "domain", TYPE_DOMAIN (node), indent + 4);
      else if (code == VECTOR_TYPE)
	fprintf (file, " nunits %d", (int) TYPE_VECTOR_SUBPARTS (node));
      else if (code == RECORD_TYPE
	       || code == UNION_TYPE
	       || code == QUAL_UNION_TYPE)
	print_node (file, "fields", TYPE_FIELDS (node), indent + 4);
      else if (code == FUNCTION_TYPE
	       || code == METHOD_TYPE)
	{
	  if (TYPE_METHOD_BASETYPE (node))
	    print_node_brief (file, "method basetype",
			      TYPE_METHOD_BASETYPE (node), indent + 4);
	  print_node (file, "arg-types", TYPE_ARG_TYPES (node), indent + 4);
	}
      else if (code == OFFSET_TYPE)
	print_node_brief (file, "basetype", TYPE_OFFSET_BASETYPE (node),
			  indent + 4);

      if (TYPE_CONTEXT (node))
	print_node_brief (file, "context", TYPE_CONTEXT (node), indent + 4);

      lang_hooks.print_type (file, node, indent);

      if (TYPE_POINTER_TO (node) || TREE_CHAIN (node))
	indent_to (file, indent + 3);

      print_node_brief (file, "pointer_to_this", TYPE_POINTER_TO (node),
			indent + 4);
      print_node_brief (file, "reference_to_this", TYPE_REFERENCE_TO (node),
			indent + 4);
      print_node_brief (file, "chain", TREE_CHAIN (node), indent + 4);
      break;

    case tcc_expression:
    case tcc_comparison:
    case tcc_unary:
    case tcc_binary:
    case tcc_reference:
    case tcc_statement:
    case tcc_vl_exp:
      if (code == BIND_EXPR)
	{
	  print_node (file, "vars", TREE_OPERAND (node, 0), indent + 4);
	  print_node (file, "body", TREE_OPERAND (node, 1), indent + 4);
	  print_node (file, "block", TREE_OPERAND (node, 2), indent + 4);
	  break;
	}
      if (code == CALL_EXPR)
	{
	  call_expr_arg_iterator iter;
	  tree arg;
	  print_node (file, "fn", CALL_EXPR_FN (node), indent + 4);
	  print_node (file, "static_chain", CALL_EXPR_STATIC_CHAIN (node),
		      indent + 4);
	  i = 0;
	  FOR_EACH_CALL_EXPR_ARG (arg, iter, node)
	    {
	      char temp[10];
	      sprintf (temp, "arg %d", i);
	      print_node (file, temp, arg, indent + 4);
	      i++;
	    }
	}
      else
	{
	  len = TREE_OPERAND_LENGTH (node);

	  for (i = 0; i < len; i++)
	    {
	      char temp[10];

	      sprintf (temp, "arg %d", i);
	      print_node (file, temp, TREE_OPERAND (node, i), indent + 4);
	    }
	}
      if (CODE_CONTAINS_STRUCT (code, TS_COMMON))
	print_node (file, "chain", TREE_CHAIN (node), indent + 4);
      break;

    case tcc_constant:
    case tcc_exceptional:
      switch (code)
	{
	case INTEGER_CST:
	  if (TREE_OVERFLOW (node))
	    fprintf (file, " overflow");

	  fprintf (file, " ");
	  print_dec (node, file, TYPE_SIGN (TREE_TYPE (node)));
	  break;

	case REAL_CST:
	  {
	    REAL_VALUE_TYPE d;

	    if (TREE_OVERFLOW (node))
	      fprintf (file, " overflow");

	    d = TREE_REAL_CST (node);
	    if (REAL_VALUE_ISINF (d))
	      fprintf (file,  REAL_VALUE_NEGATIVE (d) ? " -Inf" : " Inf");
	    else if (REAL_VALUE_ISNAN (d))
	      fprintf (file, " Nan");
	    else
	      {
		char string[64];
		real_to_decimal (string, &d, sizeof (string), 0, 1);
		fprintf (file, " %s", string);
	      }
	  }
	  break;

	case FIXED_CST:
	  {
	    FIXED_VALUE_TYPE f;
	    char string[64];

	    if (TREE_OVERFLOW (node))
	      fprintf (file, " overflow");

	    f = TREE_FIXED_CST (node);
	    fixed_to_decimal (string, &f, sizeof (string));
	    fprintf (file, " %s", string);
	  }
	  break;

	case VECTOR_CST:
	  {
	    char buf[10];
	    unsigned i;

	    for (i = 0; i < VECTOR_CST_NELTS (node); ++i)
	      {
		sprintf (buf, "elt%u: ", i);
		print_node (file, buf, VECTOR_CST_ELT (node, i), indent + 4);
	      }
	  }
	  break;

	case COMPLEX_CST:
	  print_node (file, "real", TREE_REALPART (node), indent + 4);
	  print_node (file, "imag", TREE_IMAGPART (node), indent + 4);
	  break;

	case STRING_CST:
	  {
	    const char *p = TREE_STRING_POINTER (node);
	    int i = TREE_STRING_LENGTH (node);
	    fputs (" \"", file);
	    while (--i >= 0)
	      {
		char ch = *p++;
		if (ch >= ' ' && ch < 127)
		  putc (ch, file);
		else
		  fprintf (file, "\\%03o", ch & 0xFF);
	      }
	    fputc ('\"', file);
	  }
	  break;

	case IDENTIFIER_NODE:
	  lang_hooks.print_identifier (file, node, indent);
	  break;

	case TREE_LIST:
	  print_node (file, "purpose", TREE_PURPOSE (node), indent + 4);
	  print_node (file, "value", TREE_VALUE (node), indent + 4);
	  print_node (file, "chain", TREE_CHAIN (node), indent + 4);
	  break;

	case TREE_VEC:
	  len = TREE_VEC_LENGTH (node);
	  fprintf (file, " length %d", len);
	  for (i = 0; i < len; i++)
	    if (TREE_VEC_ELT (node, i))
	      {
		char temp[10];
		sprintf (temp, "elt %d", i);
		print_node (file, temp, TREE_VEC_ELT (node, i), indent + 4);
	      }
	  break;

	case CONSTRUCTOR:
	  {
	    unsigned HOST_WIDE_INT cnt;
	    tree index, value;
	    len = vec_safe_length (CONSTRUCTOR_ELTS (node));
	    fprintf (file, " lngt %d", len);
	    FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (node),
				      cnt, index, value)
	      {
		print_node (file, "idx", index, indent + 4);
		print_node (file, "val", value, indent + 4);
	      }
	  }
	  break;

    	case STATEMENT_LIST:
	  dump_addr (file, " head ", node->stmt_list.head);
	  dump_addr (file, " tail ", node->stmt_list.tail);
	  fprintf (file, " stmts");
	  {
	    tree_stmt_iterator i;
	    for (i = tsi_start (node); !tsi_end_p (i); tsi_next (&i))
	      {
		/* Not printing the addresses of the (not-a-tree)
		   'struct tree_stmt_list_node's.  */
		dump_addr (file, " ", tsi_stmt (i));
	      }
	    fprintf (file, "\n");
	    for (i = tsi_start (node); !tsi_end_p (i); tsi_next (&i))
	      {
		/* Not printing the addresses of the (not-a-tree)
		   'struct tree_stmt_list_node's.  */
		print_node (file, "stmt", tsi_stmt (i), indent + 4);
	      }
	  }
	  break;

	case BLOCK:
	  print_node (file, "vars", BLOCK_VARS (node), indent + 4);
	  print_node (file, "supercontext", BLOCK_SUPERCONTEXT (node),
		      indent + 4);
	  print_node (file, "subblocks", BLOCK_SUBBLOCKS (node), indent + 4);
	  print_node (file, "chain", BLOCK_CHAIN (node), indent + 4);
	  print_node (file, "abstract_origin",
		      BLOCK_ABSTRACT_ORIGIN (node), indent + 4);
	  break;

	case SSA_NAME:
	  print_node_brief (file, "var", SSA_NAME_VAR (node), indent + 4);
	  fprintf (file, "def_stmt ");
	  print_gimple_stmt (file, SSA_NAME_DEF_STMT (node), indent + 4, 0);

	  indent_to (file, indent + 4);
	  fprintf (file, "version %u", SSA_NAME_VERSION (node));
	  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (node))
	    fprintf (file, " in-abnormal-phi");
	  if (SSA_NAME_IN_FREE_LIST (node))
	    fprintf (file, " in-free-list");

	  if (SSA_NAME_PTR_INFO (node))
	    {
	      indent_to (file, indent + 3);
	      if (SSA_NAME_PTR_INFO (node))
		dump_addr (file, " ptr-info ", SSA_NAME_PTR_INFO (node));
	    }
	  break;

	case OMP_CLAUSE:
	    {
	      int i;
	      fprintf (file, " %s",
		       omp_clause_code_name[OMP_CLAUSE_CODE (node)]);
	      for (i = 0; i < omp_clause_num_ops[OMP_CLAUSE_CODE (node)]; i++)
		{
		  indent_to (file, indent + 4);
		  fprintf (file, "op %d:", i);
		  print_node_brief (file, "", OMP_CLAUSE_OPERAND (node, i), 0);
		}
	    }
	  break;

	case OPTIMIZATION_NODE:
	  cl_optimization_print (file, indent + 4, TREE_OPTIMIZATION (node));
	  break;

	case TARGET_OPTION_NODE:
	  cl_target_option_print (file, indent + 4, TREE_TARGET_OPTION (node));
	  break;
	case IMPORTED_DECL:
	  fprintf (file, " imported declaration");
	  print_node_brief (file, "associated declaration",
			    IMPORTED_DECL_ASSOCIATED_DECL (node),
			    indent + 4);
	  break;

	case TREE_BINFO:
	  fprintf (file, " bases %d",
		   vec_safe_length (BINFO_BASE_BINFOS (node)));
	  print_node_brief (file, "offset", BINFO_OFFSET (node), indent + 4);
	  print_node_brief (file, "virtuals", BINFO_VIRTUALS (node),
			    indent + 4);
	  print_node_brief (file, "inheritance chain",
			    BINFO_INHERITANCE_CHAIN (node),
			    indent + 4);
	  break;

	default:
	  if (EXCEPTIONAL_CLASS_P (node))
	    lang_hooks.print_xnode (file, node, indent);
	  break;
	}

      break;
    }

  if (EXPR_HAS_LOCATION (node))
    {
      expanded_location xloc = expand_location (EXPR_LOCATION (node));
      indent_to (file, indent+4);
      fprintf (file, "%s:%d:%d", xloc.file, xloc.line, xloc.column);

      /* Print the range, if any */
      source_range r = EXPR_LOCATION_RANGE (node);
      if (r.m_start)
	{
	  xloc = expand_location (r.m_start);
	  fprintf (file, " start: %s:%d:%d", xloc.file, xloc.line, xloc.column);
	}
      else
	{
	  fprintf (file, " start: unknown");
	}
      if (r.m_finish)
	{
	  xloc = expand_location (r.m_finish);
	  fprintf (file, " finish: %s:%d:%d", xloc.file, xloc.line, xloc.column);
	}
      else
	{
	  fprintf (file, " finish: unknown");
	}
    }

  fprintf (file, ">");
}


/* Print the node NODE on standard error, for debugging.
   Most nodes referred to by this one are printed recursively
   down to a depth of six.  */

DEBUG_FUNCTION void
debug_tree (tree node)
{
  table = XCNEWVEC (struct bucket *, HASH_SIZE);
  print_node (stderr, "", node, 0);
  free (table);
  table = 0;
  putc ('\n', stderr);
}

DEBUG_FUNCTION void
debug_raw (const tree_node &ref)
{
  debug_tree (const_cast <tree> (&ref));
}

DEBUG_FUNCTION void
debug_raw (const tree_node *ptr)
{
  if (ptr)
    debug_raw (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

static void
dump_tree_via_hooks (const tree_node *ptr, int options)
{
  if (DECL_P (ptr))
    lang_hooks.print_decl (stderr, const_cast <tree_node*> (ptr), 0);
  else if (TYPE_P (ptr))
    lang_hooks.print_type (stderr, const_cast <tree_node*> (ptr), 0);
  else if (TREE_CODE (ptr) == IDENTIFIER_NODE)
    lang_hooks.print_identifier (stderr, const_cast <tree_node*> (ptr), 0);
  else
    print_generic_expr (stderr, const_cast <tree_node*> (ptr), options);
  fprintf (stderr, "\n");
}

DEBUG_FUNCTION void
debug (const tree_node &ref)
{
  dump_tree_via_hooks (&ref, 0);
}

DEBUG_FUNCTION void
debug (const tree_node *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

DEBUG_FUNCTION void
debug_verbose (const tree_node &ref)
{
  dump_tree_via_hooks (&ref, TDF_VERBOSE);
}

DEBUG_FUNCTION void
debug_verbose (const tree_node *ptr)
{
  if (ptr)
    debug_verbose (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

DEBUG_FUNCTION void
debug_head (const tree_node &ref)
{
  debug (ref);
}

DEBUG_FUNCTION void
debug_head (const tree_node *ptr)
{
  if (ptr)
    debug_head (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

DEBUG_FUNCTION void
debug_body (const tree_node &ref)
{
  if (TREE_CODE (&ref) == FUNCTION_DECL)
    dump_function_to_file (const_cast <tree_node*> (&ref), stderr, 0);
  else
    debug (ref);
}

DEBUG_FUNCTION void
debug_body (const tree_node *ptr)
{
  if (ptr)
    debug_body (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

/* Print the vector of trees VEC on standard error, for debugging.
   Most nodes referred to by this one are printed recursively
   down to a depth of six.  */

DEBUG_FUNCTION void
debug_raw (vec<tree, va_gc> &ref)
{
  tree elt;
  unsigned ix;

  /* Print the slot this node is in, and its code, and address.  */
  fprintf (stderr, "<VEC");
  dump_addr (stderr, " ", ref.address ());

  FOR_EACH_VEC_ELT (ref, ix, elt)
    {
      fprintf (stderr, "elt %d ", ix);
      debug_raw (elt);
    }
}

DEBUG_FUNCTION void
debug (vec<tree, va_gc> &ref)
{
  tree elt;
  unsigned ix;

  /* Print the slot this node is in, and its code, and address.  */
  fprintf (stderr, "<VEC");
  dump_addr (stderr, " ", ref.address ());

  FOR_EACH_VEC_ELT (ref, ix, elt)
    {
      fprintf (stderr, "elt %d ", ix);
      debug (elt);
    }
}

DEBUG_FUNCTION void
debug (vec<tree, va_gc> *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

DEBUG_FUNCTION void
debug_raw (vec<tree, va_gc> *ptr)
{
  if (ptr)
    debug_raw (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

DEBUG_FUNCTION void
debug_vec_tree (vec<tree, va_gc> *vec)
{
  debug_raw (vec);
}
