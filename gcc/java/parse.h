/* Language parser definitions for the GNU compiler for the Java(TM) language.
   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Alexandre Petit-Bianco (apbianco@cygnus.com)

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
Boston, MA 02111-1307, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

#ifndef JV_LANG_H
#define JV_LANG_H

#include "lex.h"

/* Extern global variable declarations */
extern int java_error_count;
extern struct obstack temporary_obstack;
extern struct obstack permanent_obstack;
extern int quiet_flag;

#ifndef JC1_LITE
/* Function extern to java/ */
extern int int_fits_type_p PARAMS ((tree, tree));
extern tree stabilize_reference PARAMS ((tree));
#endif

/* Macros for verbose debug info  */
#ifdef  VERBOSE_SKELETON
#define RULE( rule ) printf ( "jv_yacc:%d: rule %s\n", lineno, rule )
#else
#define RULE( rule )
#endif

#ifdef VERBOSE_SKELETON
#undef SOURCE_FRONTEND_DEBUG
#define SOURCE_FRONTEND_DEBUG(X)				\
  {if (!quiet_flag) {printf ("* "); printf X; putchar ('\n');} }
#else
#define SOURCE_FRONTEND_DEBUG(X)
#endif

/* Macro for error recovering  */
#ifdef YYDEBUG
#define RECOVERED     					\
  { if (!quiet_flag) {printf ("** Recovered\n");} }
#define DRECOVERED(s) 						\
  { if (!quiet_flag) {printf ("** Recovered (%s)\n", #s);}}
#else
#define RECOVERED
#define DRECOVERED(s)
#endif

#define DRECOVER(s) {yyerrok; DRECOVERED(s)}
#define RECOVER     {yyerrok; RECOVERED}

#define YYERROR_NOW ctxp->java_error_flag = 1
#define YYNOT_TWICE if (ctxp->prevent_ese != lineno)

/* Accepted modifiers */
#define CLASS_MODIFIERS ACC_PUBLIC|ACC_ABSTRACT|ACC_FINAL
#define FIELD_MODIFIERS ACC_PUBLIC|ACC_PROTECTED|ACC_PRIVATE|ACC_FINAL| \
                        ACC_STATIC|ACC_TRANSIENT|ACC_VOLATILE
#define METHOD_MODIFIERS ACC_PUBLIC|ACC_PROTECTED|ACC_PRIVATE|ACC_ABSTRACT| \
			 ACC_STATIC|ACC_FINAL|ACC_SYNCHRONIZED|ACC_NATIVE
#define INTERFACE_MODIFIERS ACC_PUBLIC|ACC_ABSTRACT
#define INTERFACE_METHOD_MODIFIERS ACC_PUBLIC|ACC_ABSTRACT
#define INTERFACE_FIELD_MODIFIERS ACC_PUBLIC|ACC_STATIC|ACC_FINAL

/* Getting a modifier WFL */
#define MODIFIER_WFL(M)   (ctxp->modifier_ctx [(M) - PUBLIC_TK])

/* Check on modifiers */
#define THIS_MODIFIER_ONLY(f, m, v, count, l)				\
  if ((f) & (m))							\
    {									\
      tree node = ctxp->modifier_ctx [v];				\
      if ((l)								\
	  && ((EXPR_WFL_COLNO (node) > EXPR_WFL_COLNO (l))		\
	      || (EXPR_WFL_LINENO (node) > EXPR_WFL_LINENO (l))))	\
        l = node;							\
      else if (!(l))							\
        l = node;							\
      count++;								\
    }

#define ABSTRACT_CHECK(FLAG, V, CL, S)				\
  if ((FLAG) & (V))						\
    parse_error_context ((CL), "%s method can't be abstract", (S));

#define JCONSTRUCTOR_CHECK(FLAG, V, CL, S)			\
  if ((FLAG) & (V))						\
    parse_error_context ((CL), "Constructor can't be %s", (S));	\
      
/* Misc. */
#define exit_java_complete_class()		\
  {						\
    pop_obstacks ();				\
    return;					\
  }

#define CLASS_OR_INTERFACE(decl, s1, s2)			\
   (decl ?							\
    ((get_access_flags_from_decl (TYPE_NAME (TREE_TYPE (decl)))	\
      & ACC_INTERFACE) ?					\
     s2 : s1) : ((s1 [0]=='S'|| s1 [0]=='s') ?			\
		 (s1 [0]=='S' ? "Supertype" : "supertype") :	\
		 (s1 [0] > 'A' ? "Type" : "type")))

#define GET_REAL_TYPE(TYPE) 					\
  (TREE_CODE (TYPE) == TREE_LIST ? TREE_PURPOSE (TYPE) : TYPE)

#define GET_METHOD_NAME(METHOD)					\
  (TREE_CODE (DECL_NAME (METHOD)) == EXPR_WITH_FILE_LOCATION ?	\
   EXPR_WFL_NODE (DECL_NAME (METHOD)) : DECL_NAME (METHOD))

/* Get TYPE name string, regardless whether TYPE is a class or an
   array. */
#define GET_TYPE_NAME(TYPE)				\
  (TREE_CODE (TYPE_NAME (TYPE)) == IDENTIFIER_NODE ?	\
   IDENTIFIER_POINTER (TYPE_NAME (TYPE)) :		\
   IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (TYPE))))

/* Pedantic warning on obsolete modifiers. Note: when cl is NULL,
   flags was set artificially, such as for a interface method */
#define OBSOLETE_MODIFIER_WARNING(cl, flags, __modifier, format, arg)        \
  {                                                                          \
    if (flag_redundant && (cl) && ((flags) & (__modifier)))		     \
      parse_warning_context (cl,                                             \
     "Discouraged redundant use of `%s' modifier in declaration of " format, \
			     java_accstring_lookup (__modifier), arg);       \
  }

/* Quickly build a temporary pointer on hypothetical type NAME. */
#define BUILD_PTR_FROM_NAME(ptr, name)		\
  {						\
    ptr = build (POINTER_TYPE, NULL_TREE);	\
    TYPE_NAME (ptr) = name;			\
  }

#define INCOMPLETE_TYPE_P(NODE)				\
  ((TREE_CODE (NODE) == POINTER_TYPE)			\
   && !TREE_TYPE (NODE) 				\
   && TREE_CODE (TYPE_NAME (NODE)) == IDENTIFIER_NODE)

/* Set the EMIT_LINE_NOTE flag of a EXPR_WLF to 1 if debug information
   are requested. Works in the context of a parser rule. */
#define JAVA_MAYBE_GENERATE_DEBUG_INFO(node)		\
  (debug_info_level != DINFO_LEVEL_NONE ? 		\
    EXPR_WFL_EMIT_LINE_NOTE (node) = 1, node : node)

/* Types classification, according to the JLS, section 4.2 */
#define JFLOAT_TYPE_P(TYPE)      (TYPE && TREE_CODE ((TYPE)) == REAL_TYPE)
#define JINTEGRAL_TYPE_P(TYPE)   ((TYPE) 				   \
				  && (TREE_CODE ((TYPE)) == INTEGER_TYPE   \
				      || TREE_CODE ((TYPE)) == CHAR_TYPE))
#define JNUMERIC_TYPE_P(TYPE)    ((TYPE)				\
				  && (JFLOAT_TYPE_P ((TYPE))		\
				      || JINTEGRAL_TYPE_P ((TYPE))))
#define JPRIMITIVE_TYPE_P(TYPE)  ((TYPE) 				  \
				  && (JNUMERIC_TYPE_P ((TYPE))		  \
				  || TREE_CODE ((TYPE)) == BOOLEAN_TYPE))

#define JBSC_TYPE_P(TYPE) ((TYPE) && (((TYPE) == byte_type_node)	\
				      || ((TYPE) == short_type_node)	\
				      || ((TYPE) == char_type_node)))

/* Not defined in the LRM */
#define JSTRING_TYPE_P(TYPE) ((TYPE) 					   \
			      && ((TYPE) == string_type_node ||		   \
				  (TREE_CODE (TYPE) == POINTER_TYPE &&	   \
				   TREE_TYPE (TYPE) == string_type_node)))
#define JSTRING_P(NODE) ((NODE)						\
			 && (TREE_CODE (NODE) == STRING_CST		\
			     || IS_CRAFTED_STRING_BUFFER_P (NODE)	\
			     || JSTRING_TYPE_P (TREE_TYPE (NODE))))

#define JREFERENCE_TYPE_P(TYPE) ((TYPE)					      \
				 && (TREE_CODE (TYPE) == RECORD_TYPE 	      \
				     ||	(TREE_CODE (TYPE) == POINTER_TYPE     \
					 &&  TREE_CODE (TREE_TYPE (TYPE)) ==  \
					 RECORD_TYPE)))
#define JNULLP_TYPE_P(TYPE) ((TYPE) && (TREE_CODE (TYPE) == POINTER_TYPE) \
			     && (TYPE) == TREE_TYPE (null_pointer_node))

/* Other predicate */
#define JDECL_P(NODE) (NODE && (TREE_CODE (NODE) == PARM_DECL		\
				|| TREE_CODE (NODE) == VAR_DECL		\
				|| TREE_CODE (NODE) == FIELD_DECL))

#define TYPE_INTERFACE_P(TYPE) 					\
  (CLASS_P (TYPE) && CLASS_INTERFACE (TYPE_NAME (TYPE)))

#define TYPE_CLASS_P(TYPE) (CLASS_P (TYPE) 				\
			    && !CLASS_INTERFACE (TYPE_NAME (TYPE)))

/* Standard error messages */
#define ERROR_CANT_CONVERT_TO_BOOLEAN(OPERATOR, NODE, TYPE)		\
  parse_error_context ((OPERATOR),					\
    "Incompatible type for `%s'. Can't convert `%s' to boolean",	\
    operator_string ((NODE)), lang_printable_name ((TYPE),0))

#define ERROR_CANT_CONVERT_TO_NUMERIC(OPERATOR, NODE, TYPE)		\
  parse_error_context ((OPERATOR),					\
      "Incompatible type for `%s'. Can't convert `%s' to numeric type",	\
      operator_string ((NODE)), lang_printable_name ((TYPE), 0))

#define ERROR_CAST_NEEDED_TO_INTEGRAL(OPERATOR, NODE, TYPE)		\
do {									\
  tree _operator = (OPERATOR), _node = (NODE), _type = (TYPE);		\
  if (JPRIMITIVE_TYPE_P (_type))					\
    parse_error_context (_operator,					\
"Incompatible type for `%s'. Explicit cast needed to convert `%s' to integral",\
			 operator_string(_node),			\
			 lang_printable_name (_type, 0));		\
  else									\
    parse_error_context (_operator,					\
      "Incompatible type for `%s'. Can't convert `%s' to integral",	\
			 operator_string(_node),			\
			 lang_printable_name (_type, 0));		\
} while (0)

#define ERROR_VARIABLE_NOT_INITIALIZED(WFL, V)			\
  parse_error_context						\
    ((WFL), "Variable `%s' may not have been initialized",	\
     IDENTIFIER_POINTER (V))

/* Definition for loop handling. This is Java's own definition of a
   loop body. See parse.y for documentation. It's valid once you hold
   a loop's body (LOOP_EXPR_BODY) */

/* The loop main block is the one hold the condition and the loop body */
#define LOOP_EXPR_BODY_MAIN_BLOCK(NODE) TREE_OPERAND (NODE, 0)
/* And then there is the loop update block */
#define LOOP_EXPR_BODY_UPDATE_BLOCK(NODE) TREE_OPERAND (NODE, 1)

/* Inside the loop main block, there is the loop condition and the
   loop body. They may be reversed if the loop being described is a
   do-while loop. NOTE: if you use a WFL around the EXIT_EXPR so you
   can issue debug info for it, the EXIT_EXPR will be one operand
   further. */
#define LOOP_EXPR_BODY_CONDITION_EXPR(NODE, R) 			\
  TREE_OPERAND (LOOP_EXPR_BODY_MAIN_BLOCK (NODE), (R ? 1 : 0))

/* Here is the labeled block the loop real body is encapsulated in */
#define LOOP_EXPR_BODY_LABELED_BODY(NODE, R)			\
  TREE_OPERAND (LOOP_EXPR_BODY_MAIN_BLOCK (NODE), (R ? 0 : 1))
/* And here is the loop's real body */
#define LOOP_EXPR_BODY_BODY_EXPR(NODE, R)			\
  LABELED_BLOCK_BODY (LOOP_EXPR_BODY_LABELED_BODY(NODE, R))

#define PUSH_LABELED_BLOCK(B)				\
  {							\
    TREE_CHAIN (B) = ctxp->current_labeled_block;	\
    ctxp->current_labeled_block = (B);			\
  }
#define POP_LABELED_BLOCK() 						\
  ctxp->current_labeled_block = TREE_CHAIN (ctxp->current_labeled_block)

#define PUSH_LOOP(L)				\
  {						\
    TREE_CHAIN (L) = ctxp->current_loop;	\
    ctxp->current_loop = (L);			\
  }
#define POP_LOOP() ctxp->current_loop = TREE_CHAIN (ctxp->current_loop)

#define PUSH_EXCEPTIONS(E)					\
  currently_caught_type_list =					\
    tree_cons (NULL_TREE, (E), currently_caught_type_list);

#define POP_EXCEPTIONS()						\
  currently_caught_type_list = TREE_CHAIN (currently_caught_type_list)

/* Check that we're inside a try block.  */
#define IN_TRY_BLOCK_P()				\
  (currently_caught_type_list 				\
   && ((TREE_VALUE (currently_caught_type_list) !=	\
	DECL_FUNCTION_THROWS (current_function_decl))	\
       || TREE_CHAIN (currently_caught_type_list)))

/* Check that we have exceptions in E.  */
#define EXCEPTIONS_P(E) ((E) ? TREE_VALUE (E) : NULL_TREE)

/* Invocation modes, as returned by invocation_mode (). */
enum {
  INVOKE_STATIC,
  INVOKE_NONVIRTUAL,
  INVOKE_SUPER,
  INVOKE_INTERFACE,
  INVOKE_VIRTUAL
};

/* We need the resolution stuff only if we compile jc1 */
#ifndef JC1_LITE

/* Unresolved type identifiers handling. When we process the source
   code, we blindly accept an unknown type identifier and try to
   resolve it later. When an unknown type identifier is encountered
   and used, we record in a struct jdep element what the incomplete
   type is and what it should patch. Later, java_complete_class will
   process all classes known to have unresolved type
   dependencies. Within each of these classes, this routine will
   process unresolved type dependencies (JDEP_TO_RESOLVE), patch what
   needs to be patched in the dependent tree node (JDEP_GET_PATCH,
   JDEP_APPLY_PATCH) and perform other actions dictated by the context
   of the patch (JDEP_KIND). The ideas are: we patch only what needs
   to be patched, and with java_complete_class called at the right
   time, we will start processing incomplete function bodies tree
   nodes with everything external to function's bodies already
   completed, it makes things much simpler. */

enum jdep_code {
  JDEP_NO_PATCH,		/* Must be first */
  JDEP_SUPER,			/* Patch the type of one type
				   supertype. Requires some check
				   before it's done */
  JDEP_FIELD,			/* Patch the type of a class field */

  /* JDEP_{METHOD,METHOD_RETURN,METHOD_END} to be kept in order */
  JDEP_METHOD,			/* Mark the beginning of the patching
				   of a method declaration, including
				   it's arguments */
  JDEP_METHOD_RETURN,		/* Mark the beginning of the patching
				   of a method declaration. Arguments
				   aren't patched, only the returned
				   type is */
  JDEP_METHOD_END,		/* Mark the end of the patching of a
				   method declaration. It indicates
				   that it's time to compute and
				   install a new signature */

  JDEP_INTERFACE,		/* Patch the type of a Class/interface
				   extension */
  JDEP_VARIABLE,		/* Patch the type of a variable declaration */
  JDEP_PARM,			/* Patch the type of a parm declaration */
  JDEP_TYPE,			/* Patch a random tree node type,
                                   without the need for any specific
                                   actions */
  JDEP_EXCEPTION		/* Patch exceptions specified by `throws' */
};

typedef struct _jdep {
#ifdef ONLY_INT_FIELDS
  int  kind : 8;		/* Type of patch */
#else
  enum jdep_code kind : 8;
#endif

  int  flag0 : 1;		/* Some flags */
  tree decl;			/* Tied decl/or WFL */
  tree solv;			/* What to solve */
  tree wfl;			/* Where thing to resolve where found */
  tree misc;			/* Miscellaneous info (optional). */
  tree *patch;			/* Address of a location to patch */
  struct _jdep *next;		/* Linked list */
} jdep;


#define JDEP_DECL(J)          ((J)->decl)
#define JDEP_DECL_WFL(J)      ((J)->decl)
#define JDEP_KIND(J)          ((J)->kind)
#define JDEP_SOLV(J)          ((J)->solv)
#define JDEP_WFL(J)           ((J)->wfl)
#define JDEP_MISC(J)          ((J)->misc)
#define JDEP_CLASS(J)         ((J)->class)
#define JDEP_APPLY_PATCH(J,P) (*(J)->patch = (P))
#define JDEP_GET_PATCH(J)     ((J)->patch)
#define JDEP_CHAIN(J)         ((J)->next)
#define JDEP_TO_RESOLVE(J)    ((J)->solv)
#define JDEP_RESOLVED_DECL(J) ((J)->solv)
#define JDEP_RESOLVED(J, D)   ((J)->solv = D)
#define JDEP_RESOLVED_P(J)    \
	(!(J)->solv || TREE_CODE ((J)->solv) != POINTER_TYPE)

typedef struct _jdeplist {
  jdep *first;
  jdep *last;
  struct _jdeplist *next;
} jdeplist;

#endif /* JC1_LITE */

#define CLASSD_FIRST(CD) ((CD)->first)
#define CLASSD_LAST(CD)  ((CD)->last)
#define CLASSD_CHAIN(CD) ((CD)->next)

#define JDEP_INSERT(L,J)			\
  {						\
    if (!(L)->first)				\
      (L)->last = (L)->first = (J);		\
    else					\
      {						\
	JDEP_CHAIN ((L)->last) = (J);		\
	(L)->last = (J);			\
      }						\
  }

/* if TYPE can't be resolved, obtain something suitable for its
   resolution (TYPE is saved in SAVE before being changed). and set
   CHAIN to 1. Otherwise, type is set to something usable. CHAIN is
   usually used to determine that a new DEP must be installed on TYPE.
   Note that when compiling java.lang.Object, references to Object are
   java.lang.Object.  */
#define SET_TYPE_FOR_RESOLUTION(TYPE, SAVE, CHAIN)			\
  {									\
    tree returned_type;							\
    (CHAIN) = 0;							\
    if (TREE_TYPE (ctxp->current_parsed_class) == object_type_node	\
	&& TREE_CODE (TYPE) == EXPR_WITH_FILE_LOCATION 			\
	&& EXPR_WFL_NODE (TYPE) == unqualified_object_id_node)		\
      (TYPE) = object_type_node;					\
    else								\
      {									\
	if (unresolved_type_p (type, &returned_type))			\
	  {								\
	    if (returned_type)						\
	      (TYPE) = returned_type;					\
	    else							\
	      {								\
		(SAVE) = (TYPE);					\
		(TYPE) = obtain_incomplete_type (TYPE);			\
		CHAIN = 1;						\
	      }								\
	  }								\
      }									\
  }
/* Promote a type if it won't be registered as a patch */
#define PROMOTE_RECORD_IF_COMPLETE(TYPE, IS_INCOMPLETE)		\
  {								\
    if (!(IS_INCOMPLETE) && TREE_CODE (TYPE) == RECORD_TYPE)	\
      (TYPE) = promote_type (TYPE);				\
  }

/* Insert a DECL in the current block */
#define BLOCK_CHAIN_DECL(NODE)						    \
  {		 							    \
    TREE_CHAIN ((NODE)) = 						    \
      BLOCK_EXPR_DECLS (GET_CURRENT_BLOCK (current_function_decl));         \
    BLOCK_EXPR_DECLS (GET_CURRENT_BLOCK (current_function_decl)) = (NODE);  \
  }

/* Return the current block, either found in the body of the currently
   declared function or in the current static block being defined. */
#define GET_CURRENT_BLOCK(F) ((F) ? DECL_FUNCTION_BODY ((F)) :	\
			     current_static_block)

/* For an artificial BLOCK (created to house a local variable declaration not
   at the start of an existing block), the parent block;  otherwise NULL. */
#define BLOCK_EXPR_ORIGIN(NODE) BLOCK_ABSTRACT_ORIGIN(NODE)

/* Merge an other line to the source line number of a decl. Used to
   remember function's end. */
#define DECL_SOURCE_LINE_MERGE(DECL,NO) DECL_SOURCE_LINE(DECL) |= (NO << 16)

/* Retrieve those two info separately. */
#define DECL_SOURCE_LINE_FIRST(DECL)    (DECL_SOURCE_LINE(DECL) & 0x0000ffff)
#define DECL_SOURCE_LINE_LAST(DECL)     (DECL_SOURCE_LINE(DECL) >> 16)

/* Retrieve line/column from a WFL. */
#define EXPR_WFL_GET_LINECOL(V,LINE,COL)	\
  {						\
     (LINE) = (V) >> 12;			\
     (COL) = (V) & 0xfff;			\
   }
/* Add X to the column number information */
#define EXPR_WFL_ADD_COL(V, X)					\
  (V) = (((V) & 0xfffff000) | ((((V) & 0xfff) + (X)) & 0xfff))

/* Build a WFL for expression nodes */
#define BUILD_EXPR_WFL(NODE, WFL)					\
  build_expr_wfl ((NODE), input_filename, EXPR_WFL_LINENO ((WFL)), 	\
		  EXPR_WFL_COLNO ((WFL)))

#define EXPR_WFL_QUALIFICATION(WFL) TREE_OPERAND ((WFL), 2)
#define QUAL_WFL(NODE) TREE_PURPOSE (NODE)
#define QUAL_RESOLUTION(NODE) TREE_VALUE (NODE)
#define QUAL_DECL_TYPE(NODE) GET_SKIP_TYPE (NODE)

#define GET_SKIP_TYPE(NODE)				\
  (TREE_CODE (TREE_TYPE (NODE)) == POINTER_TYPE ?	\
   TREE_TYPE (TREE_TYPE (NODE)): TREE_TYPE (NODE))

/* Handy macros for the walk operation */
#define COMPLETE_CHECK_OP(NODE, N)			\
{							\
  TREE_OPERAND ((NODE), (N)) = 				\
    java_complete_tree (TREE_OPERAND ((NODE), (N)));	\
  if (TREE_OPERAND ((NODE), (N)) == error_mark_node)	\
    return error_mark_node;				\
}
#define COMPLETE_CHECK_OP_0(NODE) COMPLETE_CHECK_OP(NODE, 0)
#define COMPLETE_CHECK_OP_1(NODE) COMPLETE_CHECK_OP(NODE, 1)
#define COMPLETE_CHECK_OP_2(NODE) COMPLETE_CHECK_OP(NODE, 2)

/* Building invocations: append(ARG) and StringBuffer(ARG) */
#define BUILD_APPEND(ARG)						      \
  ((JSTRING_TYPE_P (TREE_TYPE (ARG)) || JPRIMITIVE_TYPE_P (TREE_TYPE (ARG)))  \
   ? build_method_invocation (wfl_append,                                     \
			      ARG ? build_tree_list (NULL, (ARG)) : NULL_TREE)\
   : build_method_invocation (wfl_append,                                     \
			      ARG ? build_tree_list (NULL,                    \
						     build1 (CONVERT_EXPR,    \
							     object_type_node,\
							     (ARG)))          \
			      : NULL_TREE))
#define BUILD_STRING_BUFFER(ARG)					      \
  build_new_invocation (wfl_string_buffer, 				      \
			(ARG ? build_tree_list (NULL, (ARG)) : NULL_TREE))

/* For exception handling, build diverse function calls */
#define BUILD_ASSIGN_EXCEPTION_INFO(WHERE, TO)		\
  {							\
    (WHERE) = build (MODIFY_EXPR, void_type_node, (TO),	\
		     soft_exceptioninfo_call_node);	\
    TREE_SIDE_EFFECTS (WHERE) = 1;			\
  }

#define BUILD_THROW(WHERE, WHAT)					\
  {									\
    (WHERE) = build (CALL_EXPR, void_type_node,				\
		  build_address_of (throw_node[exceptions_via_longjmp ? 1 : 0]), \
		  build_tree_list (NULL_TREE, (WHAT)), NULL_TREE);	\
    TREE_SIDE_EFFECTS ((WHERE)) = 1;					\
  }

/* Set wfl_operator for the most accurate error location */
#define SET_WFL_OPERATOR(WHICH, NODE, WFL)		\
  EXPR_WFL_LINECOL (WHICH) =				\
    (TREE_CODE (WFL) == EXPR_WITH_FILE_LOCATION ?	\
     EXPR_WFL_LINECOL (WFL) : EXPR_WFL_LINECOL (NODE))

#define PATCH_METHOD_RETURN_ERROR()		\
  {						\
    if (ret_decl)				\
      *ret_decl = NULL_TREE;			\
    return error_mark_node;			\
  }

/* Convenient macro to check. Assumes that CLASS is a CLASS_DECL.  */
#define CHECK_METHODS(CLASS)			\
  {						\
    if (CLASS_INTERFACE ((CLASS)))		\
      java_check_abstract_methods ((CLASS));	\
    else					\
      java_check_regular_methods ((CLASS));	\
  }

/* Using and reseting the @deprecated tag flag */
#define CHECK_DEPRECATED(DECL)			\
  {						\
    if (ctxp->deprecated)			\
      DECL_DEPRECATED (DECL) = 1;		\
    ctxp->deprecated = 0;			\
  }

/* Register an import */
#define REGISTER_IMPORT(WHOLE, NAME)			\
{							\
  IS_A_SINGLE_IMPORT_CLASSFILE_NAME_P ((NAME)) = 1;	\
  node = build_tree_list ((WHOLE), (NAME));		\
  TREE_CHAIN (node) = ctxp->import_list;		\
  ctxp->import_list = node;				\
}

/* Safe check that DECL is <clinit> */
#define IS_CLINIT(DECL)				\
  (DECL != NULL_TREE && DECL_NAME (DECL) == clinit_identifier_node)

/* Macro to access the osb (opening square bracket) count */
#define CURRENT_OSB(C) (C)->osb_number [(C)->osb_depth]

/* Macro for the xreferencer */
#define DECL_END_SOURCE_LINE(DECL)       DECL_FRAME_SIZE (DECL)
#define DECL_INHERITED_SOURCE_LINE(DECL) DECL_FIELD_SIZE (DECL)
     
/* Parser context data structure. */
struct parser_ctxt {

  char *filename;		    /* Current filename */
  FILE *finput;			    /* Current file input stream */
  struct parser_ctxt *next;

  struct java_line *p_line, *c_line; /* Previous and current line */
  java_lc elc;			     /* Error's line column info */
  unicode_t unget_utf8_value;        /* An unget utf8 value */
  int ccb_indent;		     /* Keep track of {} indent, lexer */
  int first_ccb_indent1;	     /* First { at ident level 1 */
  int last_ccb_indent1;		     /* Last } at ident level 1 */
  int parser_ccb_indent;	     /* Keep track of {} indent, parser */
  int osb_depth;		     /* Current depth of [ in an expression */
  int osb_limit;		     /* Limit of this depth */
  int *osb_number;		     /* Keep track of ['s */
  int lineno;			     /* Current lineno */

  /* The flags section */

  /* Indicates a context used for saving the parser status. The
     context must be popped when the status is restored. */
  unsigned saved_data_ctx:1;	
  /* Indicates that a context already contains saved data and that the
     next save operation will require a new context to be created. */
  unsigned saved_data:1;
  /* Integral literal overflow */
  unsigned minus_seen:1;
  /* Report error when true */
  unsigned java_error_flag:1;
  /* @deprecated tag seen */
  unsigned deprecated:1;
  /* Flag to report certain errors (fix this documentation. FIXME) */
  unsigned class_err:1;

  /* This section is defined only if we compile jc1 */
#ifndef JC1_LITE
  tree modifier_ctx [11];	    /* WFL of modifiers */
  tree current_class;		    /* Current class */
  tree current_function_decl;	    /* Current function decl, save/restore */

  struct JCF *current_jcf;	    /* CU jcf */

  int prevent_ese;	            /* Prevent expression statement error */

  int formal_parameter_number;	    /* Number of parameters found */
  int interface_number;		    /* # itfs declared to extend an itf def */

  tree package;			    /* Defined package ID */

  /* Those tow list are saved accross file traversal */
  tree  incomplete_class;	    /* List of non-complete classes */
  tree  gclass_list;		    /* All classes seen from source code */

  /* These two lists won't survive file traversal */
  tree  class_list;		    /* List of classes in a CU */
  jdeplist *classd_list;	    /* Classe dependencies in a CU */
  
  tree  current_parsed_class;	    /* Class currently parsed */
  tree  current_parsed_class_un;    /* Curr. parsed class unqualified name */

  tree non_static_initialized;	    /* List of non static initialized fields */
  tree static_initialized;	    /* List of static non final initialized */

  tree import_list;		    /* List of import */
  tree import_demand_list;	    /* List of import on demand */

  tree current_loop;		    /* List of the currently nested 
				       loops/switches */
  tree current_labeled_block;	    /* List of currently nested
				       labeled blocks. */

  int pending_block;		    /* Pending block to close */

  int explicit_constructor_p;	    /* >0 when processing an explicit
				       constructor. This flag is used to trap
				       illegal argument usage during an
				       explicit constructor invocation. */
#endif /* JC1_LITE */
};

#ifndef JC1_LITE
void safe_layout_class PARAMS ((tree));
void java_complete_class PARAMS ((void));
void java_check_circular_reference PARAMS ((void));
void java_fix_constructors PARAMS ((void));
void java_check_final PARAMS ((void));
void java_layout_classes PARAMS ((void));
tree java_method_add_stmt PARAMS ((tree, tree));
void java_expand_switch PARAMS ((tree));
int java_report_errors PARAMS ((void));
extern tree do_resolve_class PARAMS ((tree, tree, tree));
#endif
char *java_get_line_col PARAMS ((char *, int, int));
extern void reset_report PARAMS ((void));

/* Always in use, no matter what you compile */
void java_push_parser_context PARAMS ((void));
void java_pop_parser_context PARAMS ((int));
void java_init_lex PARAMS ((void));
extern void java_parser_context_save_global PARAMS ((void));
extern void java_parser_context_restore_global PARAMS ((void));
int yyparse PARAMS ((void));
extern int java_parse PARAMS ((void));
void yyerror PARAMS ((const char *));
extern void java_expand_classes PARAMS ((void));
#endif
