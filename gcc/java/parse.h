/* Language parser definitions for the GNU compiler for the Java(TM) language.
   Copyright (C) 1997, 1998 Free Software Foundation, Inc.
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
extern int int_fits_type_p PROTO ((tree, tree));
extern tree stabilize_reference PROTO ((tree));
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
    parse_error_context ((CL), S " method can't be abstract");

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

/* Pedantic warning on obsolete modifiers. Note: when cl is NULL,
   flags was set artificially, such as for a interface method */
#define OBSOLETE_MODIFIER_WARNING(cl, flags, modifier, format, arg)          \
  {                                                                          \
    if ((cl) && ((flags) & (modifier)))					     \
      parse_warning_context (cl,                                             \
			     "Discouraged redundant use of `%s' modifier "   \
			     "in declaration of " format,                    \
			     java_accstring_lookup (modifier), arg);         \
  }

/* Quickly build a temporary pointer on hypothetical type NAME. */
#define BUILD_PTR_FROM_NAME(ptr, name)		\
  {						\
    ptr = build (POINTER_TYPE, NULL_TREE);	\
    TYPE_NAME (ptr) = name;			\
  }

#define INCOMPLETE_TYPE_P(NODE)					\
  ((TREE_CODE (NODE) == TREE_LIST) 				\
   && (TREE_CODE (TREE_PURPOSE (NODE)) == POINTER_TYPE) 	\
   && (TREE_TYPE (TREE_PURPOSE (NODE)) == NULL_TREE))

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
#define DECL_P(NODE) (NODE && (TREE_CODE (NODE) == PARM_DECL		\
			       || TREE_CODE (NODE) == VAR_DECL		\
			       || TREE_CODE (NODE) == FIELD_DECL))

#define TYPE_INTERFACE_P(TYPE) 					\
  (CLASS_P (TYPE) && CLASS_INTERFACE (TYPE_NAME (TYPE)))

#define TYPE_CLASS_P(TYPE) (CLASS_P (TYPE) 				\
			    && !CLASS_INTERFACE (TYPE_NAME (TYPE))	\
			    && !TYPE_ARRAY_P (TYPE))

/* Standard error messages */
#define ERROR_CANT_CONVERT_TO_BOOLEAN(OPERATOR, NODE, TYPE)		\
  parse_error_context							\
    ((OPERATOR), "Incompatible type for `%s'. Can't convert `%s' to "	\
     "boolean", operator_string ((NODE)), lang_printable_name ((TYPE),0))

#define ERROR_CANT_CONVERT_TO_NUMERIC(OPERATOR, NODE, TYPE)		\
  parse_error_context							\
    ((OPERATOR), "Incompatible type for `%s'. Can't convert `%s' to "	\
     "numeric type", operator_string ((NODE)), lang_printable_name ((TYPE), 0))

#define ERROR_CAST_NEEDED_TO_INTEGRAL(OPERATOR, NODE, TYPE)		\
  parse_error_context							\
    ((OPERATOR), (JPRIMITIVE_TYPE_P (TYPE) ?				\
     "Incompatible type for `%s'. Explicit cast needed to convert "	\
      "`%s' to integral" : "Incompatible type for `%s'. Can't convert "	\
      "`%s' to integral"), operator_string ((NODE)),			\
      lang_printable_name ((TYPE), 0))

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

/* Does a loop have a label ? */
#define LOOP_HAS_LABEL_P(LOOP)					\
  (ctxp->current_labeled_block					\
   && LABELED_BLOCK_BODY (ctxp->current_labeled_block) == (LOOP))

/* Same operation than the one performed above, but considering the
   previous labeled block */
#define LOOP_HAS_LABEL_SKIP_P(LOOP)					     \
  (ctxp->current_labeled_block						     \
   && TREE_CHAIN (ctxp->current_labeled_block)				     \
   && LABELED_BLOCK_BODY (TREE_CHAIN (ctxp->current_labeled_block)) == (LOOP))

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
  INVOKE_VIRTUAL,
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
  JDEP_EXCEPTION,		/* Patch exceptions specified by `throws' */
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
#define JDEP_TO_RESOLVE(J)    (TREE_PURPOSE ((J)->solv))
#define JDEP_RESOLVED_DECL(J) ((J)->solv ? TREE_PURPOSE ((J)->solv):NULL_TREE)
#define JDEP_RESOLVED(J, D)			\
  {						\
    TREE_PURPOSE ((J)->solv) = D;		\
    TREE_VALUE ((J)->solv) = (J)->solv;		\
  }
#define JDEP_RESOLVED_P(J)    (!(J)->solv || 				\
			       TREE_VALUE ((J)->solv) == (J)->solv)

typedef struct _jdeplist {
  jdep *first;
  jdep *last;
  struct _jdeplist *next;
} jdeplist;
static jdeplist *reverse_jdep_list ();

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
   usually used to determine that a new DEP must be installed on TYPE.  */
#define SET_TYPE_FOR_RESOLUTION(TYPE, SAVE, CHAIN)	\
  {							\
    tree returned_type;					\
    (CHAIN) = 0;					\
    if (unresolved_type_p (type, &returned_type))	\
      {							\
	if (returned_type)				\
	  (TYPE) = returned_type;			\
	else						\
	  {						\
	    (SAVE) = (TYPE);				\
	    (TYPE) = obtain_incomplete_type (TYPE);	\
	    CHAIN = 1;					\
	  }						\
      }							\
  }

/* Insert a DECL in the current block */
#define BLOCK_CHAIN_DECL(NODE)						    \
  {		 							    \
    TREE_CHAIN ((NODE)) = 						    \
      BLOCK_EXPR_DECLS (DECL_FUNCTION_BODY (current_function_decl));	    \
    BLOCK_EXPR_DECLS (DECL_FUNCTION_BODY (current_function_decl)) = (NODE); \
  }

#define BLOCK_EXPR_DECLS(NODE)  BLOCK_VARS(NODE)
#define BLOCK_EXPR_BODY(NODE)   BLOCK_SUBBLOCKS(NODE)

/* For an artificial BLOCK (created to house a local variable declaration not
   at the start of an existing block), the parent block;  otherwise NULL. */
#define BLOCK_EXPR_ORIGIN(NODE) BLOCK_ABSTRACT_ORIGIN(NODE)

/* Merge an other line to the source line number of a decl. Used to
   remember function's end. */
#define DECL_SOURCE_LINE_MERGE(DECL,NO) DECL_SOURCE_LINE(DECL) |= (NO << 16)

/* Retrieve those two info separately. */
#define DECL_SOURCE_LINE_FIRST(DECL)    (DECL_SOURCE_LINE(DECL) & 0x0000ffff)
#define DECL_SOURCE_LINE_LAST(DECL)     (DECL_SOURCE_LINE(DECL) >> 16)

/* Build a WFL for expression nodes */
#define BUILD_EXPR_WFL(NODE, WFL)					\
  build_expr_wfl ((NODE), input_filename, EXPR_WFL_LINENO ((WFL)), 	\
		  EXPR_WFL_COLNO ((WFL)))

#define EXPR_WFL_QUALIFICATION(WFL) TREE_OPERAND ((WFL), 1)
#define QUAL_WFL(NODE) TREE_PURPOSE (NODE)
#define QUAL_RESOLUTION(NODE) TREE_VALUE (NODE)
#define QUAL_DECL_TYPE(NODE) 				\
  (TREE_CODE (TREE_TYPE (NODE)) == POINTER_TYPE ?	\
   TREE_TYPE (TREE_TYPE (NODE)) : TREE_TYPE (NODE))

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
#define BUILD_APPEND(ARG)						     \
  build_method_invocation (wfl_append, 					     \
			   (ARG ? build_tree_list (NULL, (ARG)): NULL_TREE))
#define BUILD_STRING_BUFFER(ARG)					      \
  build_new_invocation (wfl_string_buffer, 				      \
			(ARG ? build_tree_list (NULL, (ARG)) : NULL_TREE))

/* For exception handling, build diverse function calls */
#define BUILD_MONITOR_ENTER(WHERE, ARG)				\
  {								\
    (WHERE) = build (CALL_EXPR, int_type_node,			\
		     build_address_of (soft_monitorenter_node),	\
		     build_tree_list (NULL_TREE, (ARG)));	\
    TREE_SIDE_EFFECTS (WHERE) = 1;				\
  }

#define BUILD_MONITOR_EXIT(WHERE, ARG)				\
  {								\
    (WHERE) = build (CALL_EXPR, int_type_node,			\
		     build_address_of (soft_monitorexit_node),	\
		     build_tree_list (NULL_TREE, (ARG)));	\
    TREE_SIDE_EFFECTS (WHERE) = 1;				\
  }

#define BUILD_ASSIGN_EXCEPTION_INFO(WHERE, TO)		\
  {							\
    (WHERE) = build (MODIFY_EXPR, void_type_node, (TO),	\
		     soft_exceptioninfo_call_node);	\
    TREE_SIDE_EFFECTS (WHERE) = 1;			\
  }

#define BUILD_THROW(WHERE, WHAT)					\
  {									\
    (WHERE) = build (CALL_EXPR, void_type_node,				\
		  build_address_of (throw_node),			\
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

/* Register an impor */
#define REGISTER_IMPORT(WHOLE, NAME)			\
{							\
  IS_A_SINGLE_IMPORT_CLASSFILE_NAME_P ((NAME)) = 1;	\
  node = build_tree_list ((WHOLE), (NAME));		\
  TREE_CHAIN (node) = ctxp->import_list;		\
  ctxp->import_list = node;				\
}
     
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
  int osb_number;		     /* Keep track of ['s */
  int minus_seen;		     /* Integral literal overflow */
  int lineno;			    /* Current lineno */
  int java_error_flag;		    /* Report error when true */
  int deprecated;		     /* @deprecated tag seen */

  /* This section is defined only if we compile jc1 */
#ifndef JC1_LITE
  tree modifier_ctx [11];	     /* WFL of modifiers */
  tree current_class;		    /* Current class */
  tree current_function_decl;	    /* Current function decl, save/restore */

  JCF *current_jcf;		    /* CU jcf */

  int prevent_ese;	            /* Prevent expression statement error */
  int class_err;		    /* Flag to report certain errors */

  int formal_parameter_number;	    /* Number of parameters found */
  int interface_number;		    /* # itfs declared to extend an itf def */

  tree package;			    /* Defined package ID */

  tree  incomplete_class;	    /* List of non-complete classes */
  tree  current_parsed_class;	    /* Class currently parsed */
  tree  current_parsed_class_un;    /* Curr. parsed class unqualified name */
  tree  class_list;		    /* List of classes in a CU */
  tree  gclass_list;		    /* All classes seen so far. */
  jdeplist *classd_list;	    /* Classe dependencies in a CU */
  
  tree non_static_initialized;	    /* List of non static initialized fields */
  tree static_initialized;	    /* List of static non final initialized */

  tree import_list;		    /* List of import */
  tree import_demand_list;	    /* List of import on demand */

  tree current_loop;		     /* List of the currently nested loops/switches */
  tree current_labeled_block;	     /* List of currently nested
					labeled blocks. */

  int pending_block;		     /* Pending block to close */

  int explicit_constructor_p;	     /* True when processing an
					explicit constructor. This flag is 
					used to trap illegal argument usage 
					during an explicit constructor
					invocation. */
#endif /* JC1_LITE */
};

/* Functions declarations */
#ifndef JC1_LITE
static char *java_accstring_lookup PROTO ((int));
static void  classitf_redefinition_error PROTO ((char *,tree, tree, tree));
static void  variable_redefinition_error PROTO ((tree, tree, tree, int));
static void  check_modifiers PROTO ((char *, int, int));
static tree  create_class PROTO ((int, tree, tree, tree));
static tree  create_interface PROTO ((int, tree, tree));
static tree  find_field PROTO ((tree, tree));
static tree lookup_field_wrapper PROTO ((tree, tree));
static int   duplicate_declaration_error_p PROTO ((tree, tree, tree));
static void  register_fields PROTO ((int, tree, tree));
static tree parser_qualified_classname PROTO ((tree));
static int  parser_check_super PROTO ((tree, tree, tree));
static int  parser_check_super_interface PROTO ((tree, tree, tree));
static void check_modifiers_consistency PROTO ((int));
static tree lookup_cl PROTO ((tree));
static tree lookup_java_method2 PROTO ((tree, tree, int));
static tree method_header PROTO ((int, tree, tree, tree));
static void fix_method_argument_names PROTO ((tree ,tree));
static tree method_declarator PROTO ((tree, tree));
static void parse_warning_context VPROTO ((tree cl, char *msg, ...));
static void issue_warning_error_from_context PROTO ((tree, char *msg, va_list));
static tree parse_jdk1_1_error PROTO ((char *));
static void complete_class_report_errors PROTO ((jdep *));
static int process_imports PROTO ((void));
static void read_import_dir PROTO ((tree));
static int find_in_imports_on_demand PROTO ((tree));
static int find_in_imports PROTO ((tree));
static int check_pkg_class_access PROTO ((tree, tree));
static tree resolve_package PROTO ((tree, tree *));
static tree lookup_package_type PROTO ((char *, int));
static tree resolve_class PROTO ((tree, tree, tree));
static tree do_resolve_class PROTO ((tree, tree, tree));
static void declare_local_variables PROTO ((int, tree, tree));
static void source_start_java_method PROTO ((tree));
static void source_end_java_method PROTO ((void));
static void expand_start_java_method PROTO ((tree));
static tree find_name_in_single_imports PROTO ((tree));
static void check_abstract_method_header PROTO ((tree));
static tree lookup_java_interface_method2 PROTO ((tree, tree));
static tree resolve_expression_name PROTO ((tree, tree *));
static tree maybe_create_class_interface_decl PROTO ((tree, tree, tree));
static int check_class_interface_creation PROTO ((int, int, tree, 
						  tree, tree, tree));
static tree patch_method_invocation PROTO ((tree, tree, tree, 
					    int *, tree *, int));
static int breakdown_qualified PROTO ((tree *, tree *, tree));
static tree resolve_and_layout PROTO ((tree, tree));
static tree resolve_no_layout PROTO ((tree, tree));
static int invocation_mode PROTO ((tree, int));
static tree find_applicable_accessible_methods_list PROTO ((int, tree, 
							    tree, tree));
static tree find_most_specific_methods_list PROTO ((tree));
static int argument_types_convertible PROTO ((tree, tree));
static tree patch_invoke PROTO ((tree, tree, tree, int));
static tree lookup_method_invoke PROTO ((int, tree, tree, tree, tree));
static tree register_incomplete_type PROTO ((int, tree, tree, tree));
static tree obtain_incomplete_type PROTO ((tree));
static tree java_complete_tree PROTO ((tree));
static void java_complete_expand_method PROTO ((tree));
static int  unresolved_type_p PROTO ((tree, tree *));
static void create_jdep_list PROTO ((struct parser_ctxt *));
static tree build_expr_block PROTO ((tree, tree));
static tree enter_block PROTO ((void));
static tree enter_a_block PROTO ((tree));
static tree exit_block PROTO ((void));
static tree lookup_name_in_blocks PROTO ((tree));
static void maybe_absorb_scoping_blocks PROTO ((void));
static tree build_method_invocation PROTO ((tree, tree));
static tree build_new_invocation PROTO ((tree, tree));
static tree build_assignment PROTO ((int, int, tree, tree));
static tree build_binop PROTO ((enum tree_code, int, tree, tree));
static int check_final_assignment PROTO ((tree ,tree));
static tree patch_assignment PROTO ((tree, tree, tree ));
static tree patch_binop PROTO ((tree, tree, tree));
static tree build_unaryop PROTO ((int, int, tree));
static tree build_incdec PROTO ((int, int, tree, int));
static tree patch_unaryop PROTO ((tree, tree));
static tree build_cast PROTO ((int, tree, tree));
static tree build_null_of_type PROTO ((tree));
static tree patch_cast PROTO ((tree, tree));
static int valid_ref_assignconv_cast_p PROTO ((tree, tree, int));
static int valid_builtin_assignconv_identity_widening_p PROTO ((tree, tree));
static int valid_cast_to_p PROTO ((tree, tree));
static int valid_method_invocation_conversion_p PROTO ((tree, tree));
static tree try_builtin_assignconv PROTO ((tree, tree, tree));
static tree try_reference_assignconv PROTO ((tree, tree));
static tree build_unresolved_array_type PROTO ((tree));
static tree build_array_from_name PROTO ((tree, tree, tree, tree *));
static tree build_array_ref PROTO ((int, tree, tree));
static tree patch_array_ref PROTO ((tree, tree, tree));
static tree make_qualified_name PROTO ((tree, tree, int));
static tree merge_qualified_name PROTO ((tree, tree));
static tree make_qualified_primary PROTO ((tree, tree, int));
static int resolve_qualified_expression_name PROTO ((tree, tree *, tree *, tree *));
static void qualify_ambiguous_name PROTO ((tree));
static void maybe_generate_clinit PROTO ((void));
static tree resolve_field_access PROTO ((tree, tree *, tree *));
static tree build_newarray_node PROTO ((tree, tree, int));
static tree patch_newarray PROTO ((tree));
static tree resolve_type_during_patch PROTO ((tree));
static int not_initialized_as_it_should_p PROTO ((tree));
static tree build_this PROTO ((int));
static tree build_return PROTO ((int, tree));
static tree patch_return PROTO ((tree));
static tree maybe_access_field PROTO ((tree, tree, tree));
static int complete_function_arguments PROTO ((tree));
static int check_for_static_method_reference PROTO ((tree, tree, tree, tree, tree));
static int not_accessible_p PROTO ((tree, tree, int));
static void check_deprecation PROTO ((tree, tree));
static int class_in_current_package PROTO ((tree));
static tree build_if_else_statement PROTO ((int, tree, tree, tree));
static tree patch_if_else_statement PROTO ((tree));
static tree add_stmt_to_compound PROTO ((tree, tree, tree));
static tree add_stmt_to_block PROTO ((tree, tree, tree));
static tree patch_exit_expr PROTO ((tree));
static tree build_labeled_block PROTO ((int, tree));
static tree generate_labeled_block PROTO (());
static tree complete_labeled_statement PROTO ((tree, tree));
static tree build_bc_statement PROTO ((int, int, tree));
static tree patch_bc_statement PROTO ((tree));
static tree patch_loop_statement PROTO ((tree));
static tree build_new_loop PROTO ((tree));
static tree build_loop_body PROTO ((int, tree, int));
static tree complete_loop_body PROTO ((int, tree, tree, int));
static tree build_debugable_stmt PROTO ((int, tree));
static tree complete_for_loop PROTO ((int, tree, tree, tree));
static tree patch_switch_statement PROTO ((tree));
static tree string_constant_concatenation PROTO ((tree, tree));
static tree build_string_concatenation PROTO ((tree, tree));
static tree patch_string_cst PROTO ((tree));
static tree patch_string PROTO ((tree));
static tree build_jump_to_finally PROTO ((tree, tree, tree, tree));
static tree build_try_statement PROTO ((int, tree, tree, tree));
static tree patch_try_statement PROTO ((tree));
static tree patch_synchronized_statement PROTO ((tree, tree));
static tree patch_throw_statement PROTO ((tree, tree));
static void check_thrown_exceptions PROTO ((int, tree));
static int check_thrown_exceptions_do PROTO ((tree));
static void purge_unchecked_exceptions PROTO ((tree));
static void check_throws_clauses PROTO ((tree, tree, tree));
static void complete_method_declaration PROTO ((tree));
static tree build_super_invocation PROTO (());
static int verify_constructor_circularity PROTO ((tree, tree));
static char *constructor_circularity_msg PROTO ((tree, tree));
static tree build_this_super_qualified_invocation PROTO ((int, tree, tree,
							  int, int));
static char *get_printable_method_name PROTO ((tree));
static tree patch_conditional_expr PROTO ((tree, tree, tree));
static void maybe_generate_finit PROTO (());
static void fix_constructors PROTO ((tree));
static int verify_constructor_super PROTO (());
static tree create_artificial_method PROTO ((tree, int, tree, tree, tree));
static void start_artificial_method_body PROTO ((tree));
static void end_artificial_method_body PROTO ((tree));
static tree generate_field_initialization_code PROTO ((tree));
static int check_method_redefinition PROTO ((tree, tree));
static int reset_method_name PROTO ((tree));
static void java_check_regular_methods PROTO ((tree));
static void java_check_abstract_methods PROTO ((tree));
static tree maybe_build_primttype_type_ref PROTO ((tree, tree));

void safe_layout_class PROTO ((tree));
void java_complete_class PROTO ((void));
void java_check_circular_reference PROTO ((void));
void java_check_final PROTO ((void));
void java_check_methods PROTO ((void));
void java_layout_classes PROTO ((void));
tree java_method_add_stmt PROTO ((tree, tree));
char *java_get_line_col PROTO ((char *, int, int));
void java_expand_switch PROTO ((tree));
tree java_get_catch_block PROTO ((tree, int));
#endif /* JC1_LITE */

/* Always in use, no matter what you compile */

void java_push_parser_context PROTO ((void));
void java_pop_parser_context PROTO ((int));
void java_init_lex PROTO ((void));
int yyparse PROTO ((void));
int yylex ();
void yyerror PROTO ((char *));

#endif
