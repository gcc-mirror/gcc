/* Process declarations and variables for C compiler.
   Copyright (C) 1988-2017 Free Software Foundation, Inc.

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

/* Process declarations and symbol lookup for C front end.
   Also constructs types; the standard scalar types at initialization,
   and structure, union, array and enum types when they are declared.  */

/* ??? not all decl nodes are given the most useful possible
   line numbers.  For example, the CONST_DECLs for enum values.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "function.h"
#include "c-tree.h"
#include "timevar.h"
#include "stringpool.h"
#include "cgraph.h"
#include "intl.h"
#include "print-tree.h"
#include "stor-layout.h"
#include "varasm.h"
#include "attribs.h"
#include "toplev.h"
#include "debug.h"
#include "c-family/c-objc.h"
#include "c-family/c-pragma.h"
#include "c-family/c-ubsan.h"
#include "c-lang.h"
#include "langhooks.h"
#include "tree-iterator.h"
#include "dumpfile.h"
#include "plugin.h"
#include "c-family/c-ada-spec.h"
#include "cilk.h"
#include "builtins.h"
#include "spellcheck-tree.h"
#include "gcc-rich-location.h"
#include "asan.h"

/* In grokdeclarator, distinguish syntactic contexts of declarators.  */
enum decl_context
{ NORMAL,			/* Ordinary declaration */
  FUNCDEF,			/* Function definition */
  PARM,				/* Declaration of parm before function body */
  FIELD,			/* Declaration inside struct or union */
  TYPENAME};			/* Typename (inside cast or sizeof)  */

/* States indicating how grokdeclarator() should handle declspecs marked
   with __attribute__((deprecated)).  An object declared as
   __attribute__((deprecated)) suppresses warnings of uses of other
   deprecated items.  */

enum deprecated_states {
  DEPRECATED_NORMAL,
  DEPRECATED_SUPPRESS
};


/* Nonzero if we have seen an invalid cross reference
   to a struct, union, or enum, but not yet printed the message.  */
tree pending_invalid_xref;

/* File and line to appear in the eventual error message.  */
location_t pending_invalid_xref_location;

/* The file and line that the prototype came from if this is an
   old-style definition; used for diagnostics in
   store_parm_decls_oldstyle.  */

static location_t current_function_prototype_locus;

/* Whether this prototype was built-in.  */

static bool current_function_prototype_built_in;

/* The argument type information of this prototype.  */

static tree current_function_prototype_arg_types;

/* The argument information structure for the function currently being
   defined.  */

static struct c_arg_info *current_function_arg_info;

/* The obstack on which parser and related data structures, which are
   not live beyond their top-level declaration or definition, are
   allocated.  */
struct obstack parser_obstack;

/* The current statement tree.  */

static GTY(()) struct stmt_tree_s c_stmt_tree;

/* State saving variables.  */
tree c_break_label;
tree c_cont_label;

/* A list of decls to be made automatically visible in each file scope.  */
static GTY(()) tree visible_builtins;

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement that specifies a return value is seen.  */

int current_function_returns_value;

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement with no argument is seen.  */

int current_function_returns_null;

/* Set to 0 at beginning of a function definition, set to 1 if
   a call to a noreturn function is seen.  */

int current_function_returns_abnormally;

/* Set to nonzero by `grokdeclarator' for a function
   whose return type is defaulted, if warnings for this are desired.  */

static int warn_about_return_type;

/* Nonzero when the current toplevel function contains a declaration
   of a nested function which is never defined.  */

static bool undef_nested_function;

/* If non-zero, implicit "omp declare target" attribute is added into the
   attribute lists.  */
int current_omp_declare_target_attribute;

/* Each c_binding structure describes one binding of an identifier to
   a decl.  All the decls in a scope - irrespective of namespace - are
   chained together by the ->prev field, which (as the name implies)
   runs in reverse order.  All the decls in a given namespace bound to
   a given identifier are chained by the ->shadowed field, which runs
   from inner to outer scopes.

   The ->decl field usually points to a DECL node, but there are two
   exceptions.  In the namespace of type tags, the bound entity is a
   RECORD_TYPE, UNION_TYPE, or ENUMERAL_TYPE node.  If an undeclared
   identifier is encountered, it is bound to error_mark_node to
   suppress further errors about that identifier in the current
   function.

   The ->u.type field stores the type of the declaration in this scope;
   if NULL, the type is the type of the ->decl field.  This is only of
   relevance for objects with external or internal linkage which may
   be redeclared in inner scopes, forming composite types that only
   persist for the duration of those scopes.  In the external scope,
   this stores the composite of all the types declared for this
   object, visible or not.  The ->inner_comp field (used only at file
   scope) stores whether an incomplete array type at file scope was
   completed at an inner scope to an array size other than 1.

   The ->u.label field is used for labels.  It points to a structure
   which stores additional information used for warnings.

   The depth field is copied from the scope structure that holds this
   decl.  It is used to preserve the proper ordering of the ->shadowed
   field (see bind()) and also for a handful of special-case checks.
   Finally, the invisible bit is true for a decl which should be
   ignored for purposes of normal name lookup, and the nested bit is
   true for a decl that's been bound a second time in an inner scope;
   in all such cases, the binding in the outer scope will have its
   invisible bit true.  */

struct GTY((chain_next ("%h.prev"))) c_binding {
  union GTY(()) {		/* first so GTY desc can use decl */
    tree GTY((tag ("0"))) type; /* the type in this scope */
    struct c_label_vars * GTY((tag ("1"))) label; /* for warnings */
  } GTY((desc ("TREE_CODE (%0.decl) == LABEL_DECL"))) u;
  tree decl;			/* the decl bound */
  tree id;			/* the identifier it's bound to */
  struct c_binding *prev;	/* the previous decl in this scope */
  struct c_binding *shadowed;	/* the innermost decl shadowed by this one */
  unsigned int depth : 28;      /* depth of this scope */
  BOOL_BITFIELD invisible : 1;  /* normal lookup should ignore this binding */
  BOOL_BITFIELD nested : 1;     /* do not set DECL_CONTEXT when popping */
  BOOL_BITFIELD inner_comp : 1; /* incomplete array completed in inner scope */
  BOOL_BITFIELD in_struct : 1;	/* currently defined as struct field */
  location_t locus;		/* location for nested bindings */
};
#define B_IN_SCOPE(b1, b2) ((b1)->depth == (b2)->depth)
#define B_IN_CURRENT_SCOPE(b) ((b)->depth == current_scope->depth)
#define B_IN_FILE_SCOPE(b) ((b)->depth == 1 /*file_scope->depth*/)
#define B_IN_EXTERNAL_SCOPE(b) ((b)->depth == 0 /*external_scope->depth*/)

/* Each C symbol points to three linked lists of c_binding structures.
   These describe the values of the identifier in the three different
   namespaces defined by the language.  */

struct GTY(()) lang_identifier {
  struct c_common_identifier common_id;
  struct c_binding *symbol_binding; /* vars, funcs, constants, typedefs */
  struct c_binding *tag_binding;    /* struct/union/enum tags */
  struct c_binding *label_binding;  /* labels */
};

/* Validate c-lang.c's assumptions.  */
extern char C_SIZEOF_STRUCT_LANG_IDENTIFIER_isnt_accurate
[(sizeof(struct lang_identifier) == C_SIZEOF_STRUCT_LANG_IDENTIFIER) ? 1 : -1];

/* The binding oracle; see c-tree.h.  */
void (*c_binding_oracle) (enum c_oracle_request, tree identifier);

/* This flag is set on an identifier if we have previously asked the
   binding oracle for this identifier's symbol binding.  */
#define I_SYMBOL_CHECKED(node) \
  (TREE_LANG_FLAG_4 (IDENTIFIER_NODE_CHECK (node)))

static inline struct c_binding* *
i_symbol_binding (tree node)
{
  struct lang_identifier *lid
    = (struct lang_identifier *) IDENTIFIER_NODE_CHECK (node);

  if (lid->symbol_binding == NULL
      && c_binding_oracle != NULL
      && !I_SYMBOL_CHECKED (node))
    {
      /* Set the "checked" flag first, to avoid infinite recursion
	 when the binding oracle calls back into gcc.  */
      I_SYMBOL_CHECKED (node) = 1;
      c_binding_oracle (C_ORACLE_SYMBOL, node);
    }

  return &lid->symbol_binding;
}

#define I_SYMBOL_BINDING(node) (*i_symbol_binding (node))

#define I_SYMBOL_DECL(node) \
 (I_SYMBOL_BINDING(node) ? I_SYMBOL_BINDING(node)->decl : 0)

/* This flag is set on an identifier if we have previously asked the
   binding oracle for this identifier's tag binding.  */
#define I_TAG_CHECKED(node) \
  (TREE_LANG_FLAG_5 (IDENTIFIER_NODE_CHECK (node)))

static inline struct c_binding **
i_tag_binding (tree node)
{
  struct lang_identifier *lid
    = (struct lang_identifier *) IDENTIFIER_NODE_CHECK (node);

  if (lid->tag_binding == NULL
      && c_binding_oracle != NULL
      && !I_TAG_CHECKED (node))
    {
      /* Set the "checked" flag first, to avoid infinite recursion
	 when the binding oracle calls back into gcc.  */
      I_TAG_CHECKED (node) = 1;
      c_binding_oracle (C_ORACLE_TAG, node);
    }

  return &lid->tag_binding;
}

#define I_TAG_BINDING(node) (*i_tag_binding (node))

#define I_TAG_DECL(node) \
 (I_TAG_BINDING(node) ? I_TAG_BINDING(node)->decl : 0)

/* This flag is set on an identifier if we have previously asked the
   binding oracle for this identifier's label binding.  */
#define I_LABEL_CHECKED(node) \
  (TREE_LANG_FLAG_6 (IDENTIFIER_NODE_CHECK (node)))

static inline struct c_binding **
i_label_binding (tree node)
{
  struct lang_identifier *lid
    = (struct lang_identifier *) IDENTIFIER_NODE_CHECK (node);

  if (lid->label_binding == NULL
      && c_binding_oracle != NULL
      && !I_LABEL_CHECKED (node))
    {
      /* Set the "checked" flag first, to avoid infinite recursion
	 when the binding oracle calls back into gcc.  */
      I_LABEL_CHECKED (node) = 1;
      c_binding_oracle (C_ORACLE_LABEL, node);
    }

  return &lid->label_binding;
}

#define I_LABEL_BINDING(node) (*i_label_binding (node))

#define I_LABEL_DECL(node) \
 (I_LABEL_BINDING(node) ? I_LABEL_BINDING(node)->decl : 0)

/* The resulting tree type.  */

union GTY((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
       chain_next ("(union lang_tree_node *) c_tree_chain_next (&%h.generic)"))) lang_tree_node
 {
  union tree_node GTY ((tag ("0"),
			desc ("tree_node_structure (&%h)")))
    generic;
  struct lang_identifier GTY ((tag ("1"))) identifier;
};

/* Track bindings and other things that matter for goto warnings.  For
   efficiency, we do not gather all the decls at the point of
   definition.  Instead, we point into the bindings structure.  As
   scopes are popped, we update these structures and gather the decls
   that matter at that time.  */

struct GTY(()) c_spot_bindings {
  /* The currently open scope which holds bindings defined when the
     label was defined or the goto statement was found.  */
  struct c_scope *scope;
  /* The bindings in the scope field which were defined at the point
     of the label or goto.  This lets us look at older or newer
     bindings in the scope, as appropriate.  */
  struct c_binding *bindings_in_scope;
  /* The number of statement expressions that have started since this
     label or goto statement was defined.  This is zero if we are at
     the same statement expression level.  It is positive if we are in
     a statement expression started since this spot.  It is negative
     if this spot was in a statement expression and we have left
     it.  */
  int stmt_exprs;
  /* Whether we started in a statement expression but are no longer in
     it.  This is set to true if stmt_exprs ever goes negative.  */
  bool left_stmt_expr;
};

/* This structure is used to keep track of bindings seen when a goto
   statement is defined.  This is only used if we see the goto
   statement before we see the label.  */

struct GTY(()) c_goto_bindings {
  /* The location of the goto statement.  */
  location_t loc;
  /* The bindings of the goto statement.  */
  struct c_spot_bindings goto_bindings;
};

typedef struct c_goto_bindings *c_goto_bindings_p;

/* The additional information we keep track of for a label binding.
   These fields are updated as scopes are popped.  */

struct GTY(()) c_label_vars {
  /* The shadowed c_label_vars, when one label shadows another (which
     can only happen using a __label__ declaration).  */
  struct c_label_vars *shadowed;
  /* The bindings when the label was defined.  */
  struct c_spot_bindings label_bindings;
  /* A list of decls that we care about: decls about which we should
     warn if a goto branches to this label from later in the function.
     Decls are added to this list as scopes are popped.  We only add
     the decls that matter.  */
  vec<tree, va_gc> *decls_in_scope;
  /* A list of goto statements to this label.  This is only used for
     goto statements seen before the label was defined, so that we can
     issue appropriate warnings for them.  */
  vec<c_goto_bindings_p, va_gc> *gotos;
};

/* Each c_scope structure describes the complete contents of one
   scope.  Four scopes are distinguished specially: the innermost or
   current scope, the innermost function scope, the file scope (always
   the second to outermost) and the outermost or external scope.

   Most declarations are recorded in the current scope.

   All normal label declarations are recorded in the innermost
   function scope, as are bindings of undeclared identifiers to
   error_mark_node.  (GCC permits nested functions as an extension,
   hence the 'innermost' qualifier.)  Explicitly declared labels
   (using the __label__ extension) appear in the current scope.

   Being in the file scope (current_scope == file_scope) causes
   special behavior in several places below.  Also, under some
   conditions the Objective-C front end records declarations in the
   file scope even though that isn't the current scope.

   All declarations with external linkage are recorded in the external
   scope, even if they aren't visible there; this models the fact that
   such declarations are visible to the entire program, and (with a
   bit of cleverness, see pushdecl) allows diagnosis of some violations
   of C99 6.2.2p7 and 6.2.7p2:

     If, within the same translation unit, the same identifier appears
     with both internal and external linkage, the behavior is
     undefined.

     All declarations that refer to the same object or function shall
     have compatible type; otherwise, the behavior is undefined.

   Initially only the built-in declarations, which describe compiler
   intrinsic functions plus a subset of the standard library, are in
   this scope.

   The order of the blocks list matters, and it is frequently appended
   to.  To avoid having to walk all the way to the end of the list on
   each insertion, or reverse the list later, we maintain a pointer to
   the last list entry.  (FIXME: It should be feasible to use a reversed
   list here.)

   The bindings list is strictly in reverse order of declarations;
   pop_scope relies on this.  */


struct GTY((chain_next ("%h.outer"))) c_scope {
  /* The scope containing this one.  */
  struct c_scope *outer;

  /* The next outermost function scope.  */
  struct c_scope *outer_function;

  /* All bindings in this scope.  */
  struct c_binding *bindings;

  /* For each scope (except the global one), a chain of BLOCK nodes
     for all the scopes that were entered and exited one level down.  */
  tree blocks;
  tree blocks_last;

  /* The depth of this scope.  Used to keep the ->shadowed chain of
     bindings sorted innermost to outermost.  */
  unsigned int depth : 28;

  /* True if we are currently filling this scope with parameter
     declarations.  */
  BOOL_BITFIELD parm_flag : 1;

  /* True if we saw [*] in this scope.  Used to give an error messages
     if these appears in a function definition.  */
  BOOL_BITFIELD had_vla_unspec : 1;

  /* True if we already complained about forward parameter decls
     in this scope.  This prevents double warnings on
     foo (int a; int b; ...)  */
  BOOL_BITFIELD warned_forward_parm_decls : 1;

  /* True if this is the outermost block scope of a function body.
     This scope contains the parameters, the local variables declared
     in the outermost block, and all the labels (except those in
     nested functions, or declared at block scope with __label__).  */
  BOOL_BITFIELD function_body : 1;

  /* True means make a BLOCK for this scope no matter what.  */
  BOOL_BITFIELD keep : 1;

  /* True means that an unsuffixed float constant is _Decimal64.  */
  BOOL_BITFIELD float_const_decimal64 : 1;

  /* True if this scope has any label bindings.  This is used to speed
     up searching for labels when popping scopes, particularly since
     labels are normally only found at function scope.  */
  BOOL_BITFIELD has_label_bindings : 1;

  /* True if we should issue a warning if a goto statement crosses any
     of the bindings.  We still need to check the list of bindings to
     find the specific ones we need to warn about.  This is true if
     decl_jump_unsafe would return true for any of the bindings.  This
     is used to avoid looping over all the bindings unnecessarily.  */
  BOOL_BITFIELD has_jump_unsafe_decl : 1;
};

/* The scope currently in effect.  */

static GTY(()) struct c_scope *current_scope;

/* The innermost function scope.  Ordinary (not explicitly declared)
   labels, bindings to error_mark_node, and the lazily-created
   bindings of __func__ and its friends get this scope.  */

static GTY(()) struct c_scope *current_function_scope;

/* The C file scope.  This is reset for each input translation unit.  */

static GTY(()) struct c_scope *file_scope;

/* The outermost scope.  This is used for all declarations with
   external linkage, and only these, hence the name.  */

static GTY(()) struct c_scope *external_scope;

/* A chain of c_scope structures awaiting reuse.  */

static GTY((deletable)) struct c_scope *scope_freelist;

/* A chain of c_binding structures awaiting reuse.  */

static GTY((deletable)) struct c_binding *binding_freelist;

/* Append VAR to LIST in scope SCOPE.  */
#define SCOPE_LIST_APPEND(scope, list, decl) do {	\
  struct c_scope *s_ = (scope);				\
  tree d_ = (decl);					\
  if (s_->list##_last)					\
    BLOCK_CHAIN (s_->list##_last) = d_;			\
  else							\
    s_->list = d_;					\
  s_->list##_last = d_;					\
} while (0)

/* Concatenate FROM in scope FSCOPE onto TO in scope TSCOPE.  */
#define SCOPE_LIST_CONCAT(tscope, to, fscope, from) do {	\
  struct c_scope *t_ = (tscope);				\
  struct c_scope *f_ = (fscope);				\
  if (t_->to##_last)						\
    BLOCK_CHAIN (t_->to##_last) = f_->from;			\
  else								\
    t_->to = f_->from;						\
  t_->to##_last = f_->from##_last;				\
} while (0)

/* A c_inline_static structure stores details of a static identifier
   referenced in a definition of a function that may be an inline
   definition if no subsequent declaration of that function uses
   "extern" or does not use "inline".  */

struct GTY((chain_next ("%h.next"))) c_inline_static {
  /* The location for a diagnostic.  */
  location_t location;

  /* The function that may be an inline definition.  */
  tree function;

  /* The object or function referenced.  */
  tree static_decl;

  /* What sort of reference this is.  */
  enum c_inline_static_type type;

  /* The next such structure or NULL.  */
  struct c_inline_static *next;
};

/* List of static identifiers used or referenced in functions that may
   be inline definitions.  */
static GTY(()) struct c_inline_static *c_inline_statics;

/* True means unconditionally make a BLOCK for the next scope pushed.  */

static bool keep_next_level_flag;

/* True means the next call to push_scope will be the outermost scope
   of a function body, so do not push a new scope, merely cease
   expecting parameter decls.  */

static bool next_is_function_body;

/* A vector of pointers to c_binding structures.  */

typedef struct c_binding *c_binding_ptr;

/* Information that we keep for a struct or union while it is being
   parsed.  */

struct c_struct_parse_info
{
  /* If warn_cxx_compat, a list of types defined within this
     struct.  */
  auto_vec<tree> struct_types;
  /* If warn_cxx_compat, a list of field names which have bindings,
     and which are defined in this struct, but which are not defined
     in any enclosing struct.  This is used to clear the in_struct
     field of the c_bindings structure.  */
  auto_vec<c_binding_ptr> fields;
  /* If warn_cxx_compat, a list of typedef names used when defining
     fields in this struct.  */
  auto_vec<tree> typedefs_seen;
};

/* Information for the struct or union currently being parsed, or
   NULL if not parsing a struct or union.  */
static struct c_struct_parse_info *struct_parse_info;

/* Forward declarations.  */
static tree lookup_name_in_scope (tree, struct c_scope *);
static tree c_make_fname_decl (location_t, tree, int);
static tree grokdeclarator (const struct c_declarator *,
			    struct c_declspecs *,
			    enum decl_context, bool, tree *, tree *, tree *,
			    bool *, enum deprecated_states);
static tree grokparms (struct c_arg_info *, bool);
static void layout_array_type (tree);
static void warn_defaults_to (location_t, int, const char *, ...)
    ATTRIBUTE_GCC_DIAG(3,4);

/* T is a statement.  Add it to the statement-tree.  This is the
   C/ObjC version--C++ has a slightly different version of this
   function.  */

tree
add_stmt (tree t)
{
  enum tree_code code = TREE_CODE (t);

  if (CAN_HAVE_LOCATION_P (t) && code != LABEL_EXPR)
    {
      if (!EXPR_HAS_LOCATION (t))
	SET_EXPR_LOCATION (t, input_location);
    }

  if (code == LABEL_EXPR || code == CASE_LABEL_EXPR)
    STATEMENT_LIST_HAS_LABEL (cur_stmt_list) = 1;

  /* Add T to the statement-tree.  Non-side-effect statements need to be
     recorded during statement expressions.  */
  if (!building_stmt_list_p ())
    push_stmt_list ();
  append_to_statement_list_force (t, &cur_stmt_list);

  return t;
}

/* Build a pointer type using the default pointer mode.  */

static tree
c_build_pointer_type (tree to_type)
{
  addr_space_t as = to_type == error_mark_node? ADDR_SPACE_GENERIC
					      : TYPE_ADDR_SPACE (to_type);
  machine_mode pointer_mode;

  if (as != ADDR_SPACE_GENERIC || c_default_pointer_mode == VOIDmode)
    pointer_mode = targetm.addr_space.pointer_mode (as);
  else
    pointer_mode = c_default_pointer_mode;
  return build_pointer_type_for_mode (to_type, pointer_mode, false);
}


/* Return true if we will want to say something if a goto statement
   crosses DECL.  */

static bool
decl_jump_unsafe (tree decl)
{
  if (error_operand_p (decl))
    return false;

  /* Always warn about crossing variably modified types.  */
  if ((VAR_P (decl) || TREE_CODE (decl) == TYPE_DECL)
      && variably_modified_type_p (TREE_TYPE (decl), NULL_TREE))
    return true;

  /* Otherwise, only warn if -Wgoto-misses-init and this is an
     initialized automatic decl.  */
  if (warn_jump_misses_init
      && VAR_P (decl)
      && !TREE_STATIC (decl)
      && DECL_INITIAL (decl) != NULL_TREE)
    return true;

  return false;
}


void
c_print_identifier (FILE *file, tree node, int indent)
{
  void (*save) (enum c_oracle_request, tree identifier);

  /* Temporarily hide any binding oracle.  Without this, calls to
     debug_tree from the debugger will end up calling into the oracle,
     making for a confusing debug session.  As the oracle isn't needed
     here for normal operation, it's simplest to suppress it.  */
  save = c_binding_oracle;
  c_binding_oracle = NULL;

  print_node (file, "symbol", I_SYMBOL_DECL (node), indent + 4);
  print_node (file, "tag", I_TAG_DECL (node), indent + 4);
  print_node (file, "label", I_LABEL_DECL (node), indent + 4);
  if (C_IS_RESERVED_WORD (node) && C_RID_CODE (node) != RID_CXX_COMPAT_WARN)
    {
      tree rid = ridpointers[C_RID_CODE (node)];
      indent_to (file, indent + 4);
      fprintf (file, "rid " HOST_PTR_PRINTF " \"%s\"",
	       (void *) rid, IDENTIFIER_POINTER (rid));
    }

  c_binding_oracle = save;
}

/* Establish a binding between NAME, an IDENTIFIER_NODE, and DECL,
   which may be any of several kinds of DECL or TYPE or error_mark_node,
   in the scope SCOPE.  */
static void
bind (tree name, tree decl, struct c_scope *scope, bool invisible,
      bool nested, location_t locus)
{
  struct c_binding *b, **here;

  if (binding_freelist)
    {
      b = binding_freelist;
      binding_freelist = b->prev;
    }
  else
    b = ggc_alloc<c_binding> ();

  b->shadowed = 0;
  b->decl = decl;
  b->id = name;
  b->depth = scope->depth;
  b->invisible = invisible;
  b->nested = nested;
  b->inner_comp = 0;
  b->in_struct = 0;
  b->locus = locus;

  b->u.type = NULL;

  b->prev = scope->bindings;
  scope->bindings = b;

  if (decl_jump_unsafe (decl))
    scope->has_jump_unsafe_decl = 1;

  if (!name)
    return;

  switch (TREE_CODE (decl))
    {
    case LABEL_DECL:     here = &I_LABEL_BINDING (name);   break;
    case ENUMERAL_TYPE:
    case UNION_TYPE:
    case RECORD_TYPE:    here = &I_TAG_BINDING (name);     break;
    case VAR_DECL:
    case FUNCTION_DECL:
    case TYPE_DECL:
    case CONST_DECL:
    case PARM_DECL:
    case ERROR_MARK:     here = &I_SYMBOL_BINDING (name);  break;

    default:
      gcc_unreachable ();
    }

  /* Locate the appropriate place in the chain of shadowed decls
     to insert this binding.  Normally, scope == current_scope and
     this does nothing.  */
  while (*here && (*here)->depth > scope->depth)
    here = &(*here)->shadowed;

  b->shadowed = *here;
  *here = b;
}

/* Clear the binding structure B, stick it on the binding_freelist,
   and return the former value of b->prev.  This is used by pop_scope
   and get_parm_info to iterate destructively over all the bindings
   from a given scope.  */
static struct c_binding *
free_binding_and_advance (struct c_binding *b)
{
  struct c_binding *prev = b->prev;

  memset (b, 0, sizeof (struct c_binding));
  b->prev = binding_freelist;
  binding_freelist = b;

  return prev;
}

/* Bind a label.  Like bind, but skip fields which aren't used for
   labels, and add the LABEL_VARS value.  */
static void
bind_label (tree name, tree label, struct c_scope *scope,
	    struct c_label_vars *label_vars)
{
  struct c_binding *b;

  bind (name, label, scope, /*invisible=*/false, /*nested=*/false,
	UNKNOWN_LOCATION);

  scope->has_label_bindings = true;

  b = scope->bindings;
  gcc_assert (b->decl == label);
  label_vars->shadowed = b->u.label;
  b->u.label = label_vars;
}

/* Hook called at end of compilation to assume 1 elt
   for a file-scope tentative array defn that wasn't complete before.  */

void
c_finish_incomplete_decl (tree decl)
{
  if (VAR_P (decl))
    {
      tree type = TREE_TYPE (decl);
      if (type != error_mark_node
	  && TREE_CODE (type) == ARRAY_TYPE
	  && !DECL_EXTERNAL (decl)
	  && TYPE_DOMAIN (type) == NULL_TREE)
	{
	  warning_at (DECL_SOURCE_LOCATION (decl),
		      0, "array %q+D assumed to have one element", decl);

	  complete_array_type (&TREE_TYPE (decl), NULL_TREE, true);

	  relayout_decl (decl);
	}
    }
}

/* Record that inline function FUNC contains a reference (location
   LOC) to static DECL (file-scope or function-local according to
   TYPE).  */

void
record_inline_static (location_t loc, tree func, tree decl,
		      enum c_inline_static_type type)
{
  c_inline_static *csi = ggc_alloc<c_inline_static> ();
  csi->location = loc;
  csi->function = func;
  csi->static_decl = decl;
  csi->type = type;
  csi->next = c_inline_statics;
  c_inline_statics = csi;
}

/* Check for references to static declarations in inline functions at
   the end of the translation unit and diagnose them if the functions
   are still inline definitions.  */

static void
check_inline_statics (void)
{
  struct c_inline_static *csi;
  for (csi = c_inline_statics; csi; csi = csi->next)
    {
      if (DECL_EXTERNAL (csi->function))
	switch (csi->type)
	  {
	  case csi_internal:
	    pedwarn (csi->location, 0,
		     "%qD is static but used in inline function %qD "
		     "which is not static", csi->static_decl, csi->function);
	    break;
	  case csi_modifiable:
	    pedwarn (csi->location, 0,
		     "%q+D is static but declared in inline function %qD "
		     "which is not static", csi->static_decl, csi->function);
	    break;
	  default:
	    gcc_unreachable ();
	  }
    }
  c_inline_statics = NULL;
}

/* Fill in a c_spot_bindings structure.  If DEFINING is true, set it
   for the current state, otherwise set it to uninitialized.  */

static void
set_spot_bindings (struct c_spot_bindings *p, bool defining)
{
  if (defining)
    {
      p->scope = current_scope;
      p->bindings_in_scope = current_scope->bindings;
    }
  else
    {
      p->scope = NULL;
      p->bindings_in_scope = NULL;
    }
  p->stmt_exprs = 0;
  p->left_stmt_expr = false;
}

/* Update spot bindings P as we pop out of SCOPE.  Return true if we
   should push decls for a label.  */

static bool
update_spot_bindings (struct c_scope *scope, struct c_spot_bindings *p)
{
  if (p->scope != scope)
    {
      /* This label or goto is defined in some other scope, or it is a
	 label which is not yet defined.  There is nothing to
	 update.  */
      return false;
    }

  /* Adjust the spot bindings to refer to the bindings already defined
     in the enclosing scope.  */
  p->scope = scope->outer;
  p->bindings_in_scope = p->scope->bindings;

  return true;
}

/* The Objective-C front-end often needs to determine the current scope.  */

void *
objc_get_current_scope (void)
{
  return current_scope;
}

/* The following function is used only by Objective-C.  It needs to live here
   because it accesses the innards of c_scope.  */

void
objc_mark_locals_volatile (void *enclosing_blk)
{
  struct c_scope *scope;
  struct c_binding *b;

  for (scope = current_scope;
       scope && scope != enclosing_blk;
       scope = scope->outer)
    {
      for (b = scope->bindings; b; b = b->prev)
	objc_volatilize_decl (b->decl);

      /* Do not climb up past the current function.  */
      if (scope->function_body)
	break;
    }
}

/* Return true if we are in the global binding level.  */

bool
global_bindings_p (void)
{
  return current_scope == file_scope;
}

void
keep_next_level (void)
{
  keep_next_level_flag = true;
}

/* Set the flag for the FLOAT_CONST_DECIMAL64 pragma being ON.  */

void
set_float_const_decimal64 (void)
{
  current_scope->float_const_decimal64 = true;
}

/* Clear the flag for the FLOAT_CONST_DECIMAL64 pragma.  */

void
clear_float_const_decimal64 (void)
{
  current_scope->float_const_decimal64 = false;
}

/* Return nonzero if an unsuffixed float constant is _Decimal64.  */

bool
float_const_decimal64_p (void)
{
  return current_scope->float_const_decimal64;
}

/* Identify this scope as currently being filled with parameters.  */

void
declare_parm_level (void)
{
  current_scope->parm_flag = true;
}

void
push_scope (void)
{
  if (next_is_function_body)
    {
      /* This is the transition from the parameters to the top level
	 of the function body.  These are the same scope
	 (C99 6.2.1p4,6) so we do not push another scope structure.
	 next_is_function_body is set only by store_parm_decls, which
	 in turn is called when and only when we are about to
	 encounter the opening curly brace for the function body.

	 The outermost block of a function always gets a BLOCK node,
	 because the debugging output routines expect that each
	 function has at least one BLOCK.  */
      current_scope->parm_flag         = false;
      current_scope->function_body     = true;
      current_scope->keep              = true;
      current_scope->outer_function    = current_function_scope;
      current_function_scope           = current_scope;

      keep_next_level_flag = false;
      next_is_function_body = false;

      /* The FLOAT_CONST_DECIMAL64 pragma applies to nested scopes.  */
      if (current_scope->outer)
	current_scope->float_const_decimal64
	  = current_scope->outer->float_const_decimal64;
      else
	current_scope->float_const_decimal64 = false;
    }
  else
    {
      struct c_scope *scope;
      if (scope_freelist)
	{
	  scope = scope_freelist;
	  scope_freelist = scope->outer;
	}
      else
	scope = ggc_cleared_alloc<c_scope> ();

      /* The FLOAT_CONST_DECIMAL64 pragma applies to nested scopes.  */
      if (current_scope)
	scope->float_const_decimal64 = current_scope->float_const_decimal64;
      else
	scope->float_const_decimal64 = false;

      scope->keep          = keep_next_level_flag;
      scope->outer         = current_scope;
      scope->depth	   = current_scope ? (current_scope->depth + 1) : 0;

      /* Check for scope depth overflow.  Unlikely (2^28 == 268,435,456) but
	 possible.  */
      if (current_scope && scope->depth == 0)
	{
	  scope->depth--;
	  sorry ("GCC supports only %u nested scopes", scope->depth);
	}

      current_scope        = scope;
      keep_next_level_flag = false;
    }
}

/* This is called when we are leaving SCOPE.  For each label defined
   in SCOPE, add any appropriate decls to its decls_in_scope fields.
   These are the decls whose initialization will be skipped by a goto
   later in the function.  */

static void
update_label_decls (struct c_scope *scope)
{
  struct c_scope *s;

  s = scope;
  while (s != NULL)
    {
      if (s->has_label_bindings)
	{
	  struct c_binding *b;

	  for (b = s->bindings; b != NULL; b = b->prev)
	    {
	      struct c_label_vars *label_vars;
	      struct c_binding *b1;
	      bool hjud;
	      unsigned int ix;
	      struct c_goto_bindings *g;

	      if (TREE_CODE (b->decl) != LABEL_DECL)
		continue;
	      label_vars = b->u.label;

	      b1 = label_vars->label_bindings.bindings_in_scope;
	      if (label_vars->label_bindings.scope == NULL)
		hjud = false;
	      else
		hjud = label_vars->label_bindings.scope->has_jump_unsafe_decl;
	      if (update_spot_bindings (scope, &label_vars->label_bindings))
		{
		  /* This label is defined in this scope.  */
		  if (hjud)
		    {
		      for (; b1 != NULL; b1 = b1->prev)
			{
			  /* A goto from later in the function to this
			     label will never see the initialization
			     of B1, if any.  Save it to issue a
			     warning if needed.  */
			  if (decl_jump_unsafe (b1->decl))
			    vec_safe_push(label_vars->decls_in_scope, b1->decl);
			}
		    }
		}

	      /* Update the bindings of any goto statements associated
		 with this label.  */
	      FOR_EACH_VEC_SAFE_ELT (label_vars->gotos, ix, g)
		update_spot_bindings (scope, &g->goto_bindings);
	    }
	}

      /* Don't search beyond the current function.  */
      if (s == current_function_scope)
	break;

      s = s->outer;
    }
}

/* Set the TYPE_CONTEXT of all of TYPE's variants to CONTEXT.  */

static void
set_type_context (tree type, tree context)
{
  for (type = TYPE_MAIN_VARIANT (type); type;
       type = TYPE_NEXT_VARIANT (type))
    TYPE_CONTEXT (type) = context;
}

/* Exit a scope.  Restore the state of the identifier-decl mappings
   that were in effect when this scope was entered.  Return a BLOCK
   node containing all the DECLs in this scope that are of interest
   to debug info generation.  */

tree
pop_scope (void)
{
  struct c_scope *scope = current_scope;
  tree block, context, p;
  struct c_binding *b;

  bool functionbody = scope->function_body;
  bool keep = functionbody || scope->keep || scope->bindings;

  update_label_decls (scope);

  /* If appropriate, create a BLOCK to record the decls for the life
     of this function.  */
  block = NULL_TREE;
  if (keep)
    {
      block = make_node (BLOCK);
      BLOCK_SUBBLOCKS (block) = scope->blocks;
      TREE_USED (block) = 1;

      /* In each subblock, record that this is its superior.  */
      for (p = scope->blocks; p; p = BLOCK_CHAIN (p))
	BLOCK_SUPERCONTEXT (p) = block;

      BLOCK_VARS (block) = NULL_TREE;
    }

  /* The TYPE_CONTEXTs for all of the tagged types belonging to this
     scope must be set so that they point to the appropriate
     construct, i.e.  either to the current FUNCTION_DECL node, or
     else to the BLOCK node we just constructed.

     Note that for tagged types whose scope is just the formal
     parameter list for some function type specification, we can't
     properly set their TYPE_CONTEXTs here, because we don't have a
     pointer to the appropriate FUNCTION_TYPE node readily available
     to us.  For those cases, the TYPE_CONTEXTs of the relevant tagged
     type nodes get set in `grokdeclarator' as soon as we have created
     the FUNCTION_TYPE node which will represent the "scope" for these
     "parameter list local" tagged types.  */
  if (scope->function_body)
    context = current_function_decl;
  else if (scope == file_scope)
    {
      tree file_decl
	= build_translation_unit_decl (get_identifier (main_input_filename));
      context = file_decl;
      debug_hooks->register_main_translation_unit (file_decl);
    }
  else
    context = block;

  /* Clear all bindings in this scope.  */
  for (b = scope->bindings; b; b = free_binding_and_advance (b))
    {
      p = b->decl;
      switch (TREE_CODE (p))
	{
	case LABEL_DECL:
	  /* Warnings for unused labels, errors for undefined labels.  */
	  if (TREE_USED (p) && !DECL_INITIAL (p))
	    {
	      error ("label %q+D used but not defined", p);
	      DECL_INITIAL (p) = error_mark_node;
	    }
	  else
	    warn_for_unused_label (p);

	  /* Labels go in BLOCK_VARS.  */
	  DECL_CHAIN (p) = BLOCK_VARS (block);
	  BLOCK_VARS (block) = p;
	  gcc_assert (I_LABEL_BINDING (b->id) == b);
	  I_LABEL_BINDING (b->id) = b->shadowed;

	  /* Also pop back to the shadowed label_vars.  */
	  release_tree_vector (b->u.label->decls_in_scope);
	  b->u.label = b->u.label->shadowed;
	  break;

	case ENUMERAL_TYPE:
	case UNION_TYPE:
	case RECORD_TYPE:
	  set_type_context (p, context);

	  /* Types may not have tag-names, in which case the type
	     appears in the bindings list with b->id NULL.  */
	  if (b->id)
	    {
	      gcc_assert (I_TAG_BINDING (b->id) == b);
	      I_TAG_BINDING (b->id) = b->shadowed;
	    }
	  break;

	case FUNCTION_DECL:
	  /* Propagate TREE_ADDRESSABLE from nested functions to their
	     containing functions.  */
	  if (!TREE_ASM_WRITTEN (p)
	      && DECL_INITIAL (p) != NULL_TREE
	      && TREE_ADDRESSABLE (p)
	      && DECL_ABSTRACT_ORIGIN (p) != NULL_TREE
	      && DECL_ABSTRACT_ORIGIN (p) != p)
	    TREE_ADDRESSABLE (DECL_ABSTRACT_ORIGIN (p)) = 1;
	  if (!DECL_EXTERNAL (p)
	      && !DECL_INITIAL (p)
	      && scope != file_scope
	      && scope != external_scope)
	    {
	      error ("nested function %q+D declared but never defined", p);
	      undef_nested_function = true;
	    }
	  else if (DECL_DECLARED_INLINE_P (p)
		   && TREE_PUBLIC (p)
		   && !DECL_INITIAL (p))
	    {
	      /* C99 6.7.4p6: "a function with external linkage... declared
		 with an inline function specifier ... shall also be defined
		 in the same translation unit."  */
	      if (!flag_gnu89_inline
		  && !lookup_attribute ("gnu_inline", DECL_ATTRIBUTES (p))
		  && scope != external_scope)
		pedwarn (input_location, 0,
			 "inline function %q+D declared but never defined", p);
	      DECL_EXTERNAL (p) = 1;
	    }

	  goto common_symbol;

	case VAR_DECL:
	  /* Warnings for unused variables.  */
	  if ((!TREE_USED (p) || !DECL_READ_P (p))
	      && !TREE_NO_WARNING (p)
	      && !DECL_IN_SYSTEM_HEADER (p)
	      && DECL_NAME (p)
	      && !DECL_ARTIFICIAL (p)
	      && scope != file_scope
	      && scope != external_scope)
	    {
	      if (!TREE_USED (p))
		warning (OPT_Wunused_variable, "unused variable %q+D", p);
	      else if (DECL_CONTEXT (p) == current_function_decl)
		warning_at (DECL_SOURCE_LOCATION (p),
			    OPT_Wunused_but_set_variable,
			    "variable %qD set but not used", p);
	    }

	  if (b->inner_comp)
	    {
	      error ("type of array %q+D completed incompatibly with"
		     " implicit initialization", p);
	    }

	  /* Fall through.  */
	case TYPE_DECL:
	case CONST_DECL:
	common_symbol:
	  /* All of these go in BLOCK_VARS, but only if this is the
	     binding in the home scope.  */
	  if (!b->nested)
	    {
	      DECL_CHAIN (p) = BLOCK_VARS (block);
	      BLOCK_VARS (block) = p;
	    }
	  else if (VAR_OR_FUNCTION_DECL_P (p) && scope != file_scope)
	    {
	      /* For block local externs add a special
		 DECL_EXTERNAL decl for debug info generation.  */
	      tree extp = copy_node (p);

	      DECL_EXTERNAL (extp) = 1;
	      TREE_STATIC (extp) = 0;
	      TREE_PUBLIC (extp) = 1;
	      DECL_INITIAL (extp) = NULL_TREE;
	      DECL_LANG_SPECIFIC (extp) = NULL;
	      DECL_CONTEXT (extp) = current_function_decl;
	      if (TREE_CODE (p) == FUNCTION_DECL)
		{
		  DECL_RESULT (extp) = NULL_TREE;
		  DECL_SAVED_TREE (extp) = NULL_TREE;
		  DECL_STRUCT_FUNCTION (extp) = NULL;
		}
	      if (b->locus != UNKNOWN_LOCATION)
		DECL_SOURCE_LOCATION (extp) = b->locus;
	      DECL_CHAIN (extp) = BLOCK_VARS (block);
	      BLOCK_VARS (block) = extp;
	    }
	  /* If this is the file scope set DECL_CONTEXT of each decl to
	     the TRANSLATION_UNIT_DECL.  This makes same_translation_unit_p
	     work.  */
	  if (scope == file_scope)
	    {
	      DECL_CONTEXT (p) = context;
	      if (TREE_CODE (p) == TYPE_DECL
		  && TREE_TYPE (p) != error_mark_node)
		set_type_context (TREE_TYPE (p), context);
	    }

	  gcc_fallthrough ();
	  /* Parameters go in DECL_ARGUMENTS, not BLOCK_VARS, and have
	     already been put there by store_parm_decls.  Unused-
	     parameter warnings are handled by function.c.
	     error_mark_node obviously does not go in BLOCK_VARS and
	     does not get unused-variable warnings.  */
	case PARM_DECL:
	case ERROR_MARK:
	  /* It is possible for a decl not to have a name.  We get
	     here with b->id NULL in this case.  */
	  if (b->id)
	    {
	      gcc_assert (I_SYMBOL_BINDING (b->id) == b);
	      I_SYMBOL_BINDING (b->id) = b->shadowed;
	      if (b->shadowed && b->shadowed->u.type)
		TREE_TYPE (b->shadowed->decl) = b->shadowed->u.type;
	    }
	  break;

	default:
	  gcc_unreachable ();
	}
    }


  /* Dispose of the block that we just made inside some higher level.  */
  if ((scope->function_body || scope == file_scope) && context)
    {
      DECL_INITIAL (context) = block;
      BLOCK_SUPERCONTEXT (block) = context;
    }
  else if (scope->outer)
    {
      if (block)
	SCOPE_LIST_APPEND (scope->outer, blocks, block);
      /* If we did not make a block for the scope just exited, any
	 blocks made for inner scopes must be carried forward so they
	 will later become subblocks of something else.  */
      else if (scope->blocks)
	SCOPE_LIST_CONCAT (scope->outer, blocks, scope, blocks);
    }

  /* Pop the current scope, and free the structure for reuse.  */
  current_scope = scope->outer;
  if (scope->function_body)
    current_function_scope = scope->outer_function;

  memset (scope, 0, sizeof (struct c_scope));
  scope->outer = scope_freelist;
  scope_freelist = scope;

  return block;
}

void
push_file_scope (void)
{
  tree decl;

  if (file_scope)
    return;

  push_scope ();
  file_scope = current_scope;

  start_fname_decls ();

  for (decl = visible_builtins; decl; decl = DECL_CHAIN (decl))
    bind (DECL_NAME (decl), decl, file_scope,
	  /*invisible=*/false, /*nested=*/true, DECL_SOURCE_LOCATION (decl));
}

void
pop_file_scope (void)
{
  /* In case there were missing closebraces, get us back to the global
     binding level.  */
  while (current_scope != file_scope)
    pop_scope ();

  /* __FUNCTION__ is defined at file scope ("").  This
     call may not be necessary as my tests indicate it
     still works without it.  */
  finish_fname_decls ();

  check_inline_statics ();

  /* This is the point to write out a PCH if we're doing that.
     In that case we do not want to do anything else.  */
  if (pch_file)
    {
      c_common_write_pch ();
      /* Ensure even the callers don't try to finalize the CU.  */
      flag_syntax_only = 1;
      return;
    }

  /* Pop off the file scope and close this translation unit.  */
  pop_scope ();
  file_scope = 0;

  maybe_apply_pending_pragma_weaks ();
}

/* Adjust the bindings for the start of a statement expression.  */

void
c_bindings_start_stmt_expr (struct c_spot_bindings* switch_bindings)
{
  struct c_scope *scope;

  for (scope = current_scope; scope != NULL; scope = scope->outer)
    {
      struct c_binding *b;

      if (!scope->has_label_bindings)
	continue;

      for (b = scope->bindings; b != NULL; b = b->prev)
	{
	  struct c_label_vars *label_vars;
	  unsigned int ix;
	  struct c_goto_bindings *g;

	  if (TREE_CODE (b->decl) != LABEL_DECL)
	    continue;
	  label_vars = b->u.label;
	  ++label_vars->label_bindings.stmt_exprs;
	  FOR_EACH_VEC_SAFE_ELT (label_vars->gotos, ix, g)
	    ++g->goto_bindings.stmt_exprs;
	}
    }

  if (switch_bindings != NULL)
    ++switch_bindings->stmt_exprs;
}

/* Adjust the bindings for the end of a statement expression.  */

void
c_bindings_end_stmt_expr (struct c_spot_bindings *switch_bindings)
{
  struct c_scope *scope;

  for (scope = current_scope; scope != NULL; scope = scope->outer)
    {
      struct c_binding *b;

      if (!scope->has_label_bindings)
	continue;

      for (b = scope->bindings; b != NULL; b = b->prev)
	{
	  struct c_label_vars *label_vars;
	  unsigned int ix;
	  struct c_goto_bindings *g;

	  if (TREE_CODE (b->decl) != LABEL_DECL)
	    continue;
	  label_vars = b->u.label;
	  --label_vars->label_bindings.stmt_exprs;
	  if (label_vars->label_bindings.stmt_exprs < 0)
	    {
	      label_vars->label_bindings.left_stmt_expr = true;
	      label_vars->label_bindings.stmt_exprs = 0;
	    }
	  FOR_EACH_VEC_SAFE_ELT (label_vars->gotos, ix, g)
	    {
	      --g->goto_bindings.stmt_exprs;
	      if (g->goto_bindings.stmt_exprs < 0)
		{
		  g->goto_bindings.left_stmt_expr = true;
		  g->goto_bindings.stmt_exprs = 0;
		}
	    }
	}
    }

  if (switch_bindings != NULL)
    {
      --switch_bindings->stmt_exprs;
      gcc_assert (switch_bindings->stmt_exprs >= 0);
    }
}

/* Push a definition or a declaration of struct, union or enum tag "name".
   "type" should be the type node.
   We assume that the tag "name" is not already defined, and has a location
   of LOC.

   Note that the definition may really be just a forward reference.
   In that case, the TYPE_SIZE will be zero.  */

static void
pushtag (location_t loc, tree name, tree type)
{
  /* Record the identifier as the type's name if it has none.  */
  if (name && !TYPE_NAME (type))
    TYPE_NAME (type) = name;
  bind (name, type, current_scope, /*invisible=*/false, /*nested=*/false, loc);

  /* Create a fake NULL-named TYPE_DECL node whose TREE_TYPE will be the
     tagged type we just added to the current scope.  This fake
     NULL-named TYPE_DECL node helps dwarfout.c to know when it needs
     to output a representation of a tagged type, and it also gives
     us a convenient place to record the "scope start" address for the
     tagged type.  */

  TYPE_STUB_DECL (type) = pushdecl (build_decl (loc,
						TYPE_DECL, NULL_TREE, type));

  /* An approximation for now, so we can tell this is a function-scope tag.
     This will be updated in pop_scope.  */
  TYPE_CONTEXT (type) = DECL_CONTEXT (TYPE_STUB_DECL (type));

  if (warn_cxx_compat && name != NULL_TREE)
    {
      struct c_binding *b = I_SYMBOL_BINDING (name);

      if (b != NULL
	  && b->decl != NULL_TREE
	  && TREE_CODE (b->decl) == TYPE_DECL
	  && (B_IN_CURRENT_SCOPE (b)
	      || (current_scope == file_scope && B_IN_EXTERNAL_SCOPE (b)))
	  && (TYPE_MAIN_VARIANT (TREE_TYPE (b->decl))
	      != TYPE_MAIN_VARIANT (type)))
	{
	  if (warning_at (loc, OPT_Wc___compat,
			  ("using %qD as both a typedef and a tag is "
			   "invalid in C++"), b->decl)
	      && b->locus != UNKNOWN_LOCATION)
	    inform (b->locus, "originally defined here");
	}
    }
}

/* An exported interface to pushtag.  This is used by the gdb plugin's
   binding oracle to introduce a new tag binding.  */

void
c_pushtag (location_t loc, tree name, tree type)
{
  pushtag (loc, name, type);
}

/* An exported interface to bind a declaration.  LOC is the location
   to use.  DECL is the declaration to bind.  The decl's name is used
   to determine how it is bound.  If DECL is a VAR_DECL, then
   IS_GLOBAL determines whether the decl is put into the global (file
   and external) scope or the current function's scope; if DECL is not
   a VAR_DECL then it is always put into the file scope.  */

void
c_bind (location_t loc, tree decl, bool is_global)
{
  struct c_scope *scope;
  bool nested = false;

  if (!VAR_P (decl) || current_function_scope == NULL)
    {
      /* Types and functions are always considered to be global.  */
      scope = file_scope;
      DECL_EXTERNAL (decl) = 1;
      TREE_PUBLIC (decl) = 1;
    }
  else if (is_global)
    {
      /* Also bind it into the external scope.  */
      bind (DECL_NAME (decl), decl, external_scope, true, false, loc);
      nested = true;
      scope = file_scope;
      DECL_EXTERNAL (decl) = 1;
      TREE_PUBLIC (decl) = 1;
    }
  else
    {
      DECL_CONTEXT (decl) = current_function_decl;
      TREE_PUBLIC (decl) = 0;
      scope = current_function_scope;
    }

  bind (DECL_NAME (decl), decl, scope, false, nested, loc);
}

/* Subroutine of compare_decls.  Allow harmless mismatches in return
   and argument types provided that the type modes match.  This function
   return a unified type given a suitable match, and 0 otherwise.  */

static tree
match_builtin_function_types (tree newtype, tree oldtype)
{
  tree newrettype, oldrettype;
  tree newargs, oldargs;
  tree trytype, tryargs;

  /* Accept the return type of the new declaration if same modes.  */
  oldrettype = TREE_TYPE (oldtype);
  newrettype = TREE_TYPE (newtype);

  if (TYPE_MODE (oldrettype) != TYPE_MODE (newrettype))
    return NULL_TREE;

  oldargs = TYPE_ARG_TYPES (oldtype);
  newargs = TYPE_ARG_TYPES (newtype);
  tryargs = newargs;

  while (oldargs || newargs)
    {
      if (!oldargs
	  || !newargs
	  || !TREE_VALUE (oldargs)
	  || !TREE_VALUE (newargs)
	  || TYPE_MODE (TREE_VALUE (oldargs))
	     != TYPE_MODE (TREE_VALUE (newargs)))
	return NULL_TREE;

      oldargs = TREE_CHAIN (oldargs);
      newargs = TREE_CHAIN (newargs);
    }

  trytype = build_function_type (newrettype, tryargs);

  /* Allow declaration to change transaction_safe attribute.  */
  tree oldattrs = TYPE_ATTRIBUTES (oldtype);
  tree oldtsafe = lookup_attribute ("transaction_safe", oldattrs);
  tree newattrs = TYPE_ATTRIBUTES (newtype);
  tree newtsafe = lookup_attribute ("transaction_safe", newattrs);
  if (oldtsafe && !newtsafe)
    oldattrs = remove_attribute ("transaction_safe", oldattrs);
  else if (newtsafe && !oldtsafe)
    oldattrs = tree_cons (get_identifier ("transaction_safe"),
			  NULL_TREE, oldattrs);

  return build_type_attribute_variant (trytype, oldattrs);
}

/* Subroutine of diagnose_mismatched_decls.  Check for function type
   mismatch involving an empty arglist vs a nonempty one and give clearer
   diagnostics.  */
static void
diagnose_arglist_conflict (tree newdecl, tree olddecl,
			   tree newtype, tree oldtype)
{
  tree t;

  if (TREE_CODE (olddecl) != FUNCTION_DECL
      || !comptypes (TREE_TYPE (oldtype), TREE_TYPE (newtype))
      || !((!prototype_p (oldtype) && DECL_INITIAL (olddecl) == NULL_TREE)
	   || (!prototype_p (newtype) && DECL_INITIAL (newdecl) == NULL_TREE)))
    return;

  t = TYPE_ARG_TYPES (oldtype);
  if (t == NULL_TREE)
    t = TYPE_ARG_TYPES (newtype);
  for (; t; t = TREE_CHAIN (t))
    {
      tree type = TREE_VALUE (t);

      if (TREE_CHAIN (t) == NULL_TREE
	  && TYPE_MAIN_VARIANT (type) != void_type_node)
	{
	  inform (input_location, "a parameter list with an ellipsis can%'t match "
		  "an empty parameter name list declaration");
	  break;
	}

      if (c_type_promotes_to (type) != type)
	{
	  inform (input_location, "an argument type that has a default promotion can%'t match "
		  "an empty parameter name list declaration");
	  break;
	}
    }
}

/* Another subroutine of diagnose_mismatched_decls.  OLDDECL is an
   old-style function definition, NEWDECL is a prototype declaration.
   Diagnose inconsistencies in the argument list.  Returns TRUE if
   the prototype is compatible, FALSE if not.  */
static bool
validate_proto_after_old_defn (tree newdecl, tree newtype, tree oldtype)
{
  tree newargs, oldargs;
  int i;

#define END_OF_ARGLIST(t) ((t) == void_type_node)

  oldargs = TYPE_ACTUAL_ARG_TYPES (oldtype);
  newargs = TYPE_ARG_TYPES (newtype);
  i = 1;

  for (;;)
    {
      tree oldargtype = TREE_VALUE (oldargs);
      tree newargtype = TREE_VALUE (newargs);

      if (oldargtype == error_mark_node || newargtype == error_mark_node)
	return false;

      oldargtype = (TYPE_ATOMIC (oldargtype)
		    ? c_build_qualified_type (TYPE_MAIN_VARIANT (oldargtype),
					      TYPE_QUAL_ATOMIC)
		    : TYPE_MAIN_VARIANT (oldargtype));
      newargtype = (TYPE_ATOMIC (newargtype)
		    ? c_build_qualified_type (TYPE_MAIN_VARIANT (newargtype),
					      TYPE_QUAL_ATOMIC)
		    : TYPE_MAIN_VARIANT (newargtype));

      if (END_OF_ARGLIST (oldargtype) && END_OF_ARGLIST (newargtype))
	break;

      /* Reaching the end of just one list means the two decls don't
	 agree on the number of arguments.  */
      if (END_OF_ARGLIST (oldargtype))
	{
	  error ("prototype for %q+D declares more arguments "
		 "than previous old-style definition", newdecl);
	  return false;
	}
      else if (END_OF_ARGLIST (newargtype))
	{
	  error ("prototype for %q+D declares fewer arguments "
		 "than previous old-style definition", newdecl);
	  return false;
	}

      /* Type for passing arg must be consistent with that declared
	 for the arg.  */
      else if (!comptypes (oldargtype, newargtype))
	{
	  error ("prototype for %q+D declares argument %d"
		 " with incompatible type",
		 newdecl, i);
	  return false;
	}

      oldargs = TREE_CHAIN (oldargs);
      newargs = TREE_CHAIN (newargs);
      i++;
    }

  /* If we get here, no errors were found, but do issue a warning
     for this poor-style construct.  */
  warning (0, "prototype for %q+D follows non-prototype definition",
	   newdecl);
  return true;
#undef END_OF_ARGLIST
}

/* Subroutine of diagnose_mismatched_decls.  Report the location of DECL,
   first in a pair of mismatched declarations, using the diagnostic
   function DIAG.  */
static void
locate_old_decl (tree decl)
{
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_BUILT_IN (decl)
      && !C_DECL_DECLARED_BUILTIN (decl))
    ;
  else if (DECL_INITIAL (decl))
    inform (input_location, "previous definition of %q+D was here", decl);
  else if (C_DECL_IMPLICIT (decl))
    inform (input_location, "previous implicit declaration of %q+D was here", decl);
  else
    inform (input_location, "previous declaration of %q+D was here", decl);
}

/* Subroutine of duplicate_decls.  Compare NEWDECL to OLDDECL.
   Returns true if the caller should proceed to merge the two, false
   if OLDDECL should simply be discarded.  As a side effect, issues
   all necessary diagnostics for invalid or poor-style combinations.
   If it returns true, writes the types of NEWDECL and OLDDECL to
   *NEWTYPEP and *OLDTYPEP - these may have been adjusted from
   TREE_TYPE (NEWDECL, OLDDECL) respectively.  */

static bool
diagnose_mismatched_decls (tree newdecl, tree olddecl,
			   tree *newtypep, tree *oldtypep)
{
  tree newtype, oldtype;
  bool pedwarned = false;
  bool warned = false;
  bool retval = true;

#define DECL_EXTERN_INLINE(DECL) (DECL_DECLARED_INLINE_P (DECL)  \
				  && DECL_EXTERNAL (DECL))

  /* If we have error_mark_node for either decl or type, just discard
     the previous decl - we're in an error cascade already.  */
  if (olddecl == error_mark_node || newdecl == error_mark_node)
    return false;
  *oldtypep = oldtype = TREE_TYPE (olddecl);
  *newtypep = newtype = TREE_TYPE (newdecl);
  if (oldtype == error_mark_node || newtype == error_mark_node)
    return false;

  /* Two different categories of symbol altogether.  This is an error
     unless OLDDECL is a builtin.  OLDDECL will be discarded in any case.  */
  if (TREE_CODE (olddecl) != TREE_CODE (newdecl))
    {
      if (!(TREE_CODE (olddecl) == FUNCTION_DECL
	    && DECL_BUILT_IN (olddecl)
	    && !C_DECL_DECLARED_BUILTIN (olddecl)))
	{
	  error ("%q+D redeclared as different kind of symbol", newdecl);
	  locate_old_decl (olddecl);
	}
      else if (TREE_PUBLIC (newdecl))
	warning (OPT_Wbuiltin_declaration_mismatch,
		 "built-in function %q+D declared as non-function",
		 newdecl);
      else
	warning (OPT_Wshadow, "declaration of %q+D shadows "
		 "a built-in function", newdecl);
      return false;
    }

  /* Enumerators have no linkage, so may only be declared once in a
     given scope.  */
  if (TREE_CODE (olddecl) == CONST_DECL)
    {
      error ("redeclaration of enumerator %q+D", newdecl);
      locate_old_decl (olddecl);
      return false;
    }

  if (!comptypes (oldtype, newtype))
    {
      if (TREE_CODE (olddecl) == FUNCTION_DECL
	  && DECL_BUILT_IN (olddecl) && !C_DECL_DECLARED_BUILTIN (olddecl))
	{
	  /* Accept harmless mismatch in function types.
	     This is for the ffs and fprintf builtins.  */
	  tree trytype = match_builtin_function_types (newtype, oldtype);

	  if (trytype && comptypes (newtype, trytype))
	    *oldtypep = oldtype = trytype;
	  else
	    {
	      /* If types don't match for a built-in, throw away the
		 built-in.  No point in calling locate_old_decl here, it
		 won't print anything.  */
	      warning (OPT_Wbuiltin_declaration_mismatch,
		       "conflicting types for built-in function %q+D",
		       newdecl);
	      return false;
	    }
	}
      else if (TREE_CODE (olddecl) == FUNCTION_DECL
	       && DECL_IS_BUILTIN (olddecl))
	{
	  /* A conflicting function declaration for a predeclared
	     function that isn't actually built in.  Objective C uses
	     these.  The new declaration silently overrides everything
	     but the volatility (i.e. noreturn) indication.  See also
	     below.  FIXME: Make Objective C use normal builtins.  */
	  TREE_THIS_VOLATILE (newdecl) |= TREE_THIS_VOLATILE (olddecl);
	  return false;
	}
      /* Permit void foo (...) to match int foo (...) if the latter is
	 the definition and implicit int was used.  See
	 c-torture/compile/920625-2.c.  */
      else if (TREE_CODE (newdecl) == FUNCTION_DECL && DECL_INITIAL (newdecl)
	       && TYPE_MAIN_VARIANT (TREE_TYPE (oldtype)) == void_type_node
	       && TYPE_MAIN_VARIANT (TREE_TYPE (newtype)) == integer_type_node
	       && C_FUNCTION_IMPLICIT_INT (newdecl) && !DECL_INITIAL (olddecl))
	{
	  pedwarned = pedwarn (input_location, 0,
			       "conflicting types for %q+D", newdecl);
	  /* Make sure we keep void as the return type.  */
	  TREE_TYPE (newdecl) = *newtypep = newtype = oldtype;
	  C_FUNCTION_IMPLICIT_INT (newdecl) = 0;
	}
      /* Permit void foo (...) to match an earlier call to foo (...) with
	 no declared type (thus, implicitly int).  */
      else if (TREE_CODE (newdecl) == FUNCTION_DECL
	       && TYPE_MAIN_VARIANT (TREE_TYPE (newtype)) == void_type_node
	       && TYPE_MAIN_VARIANT (TREE_TYPE (oldtype)) == integer_type_node
	       && C_DECL_IMPLICIT (olddecl) && !DECL_INITIAL (olddecl))
	{
	  pedwarned = pedwarn (input_location, 0,
			       "conflicting types for %q+D", newdecl);
	  /* Make sure we keep void as the return type.  */
	  TREE_TYPE (olddecl) = *oldtypep = oldtype = newtype;
	}
      else
	{
	  int new_quals = TYPE_QUALS (newtype);
	  int old_quals = TYPE_QUALS (oldtype);

	  if (new_quals != old_quals)
	    {
	      addr_space_t new_addr = DECODE_QUAL_ADDR_SPACE (new_quals);
	      addr_space_t old_addr = DECODE_QUAL_ADDR_SPACE (old_quals);
	      if (new_addr != old_addr)
		{
		  if (ADDR_SPACE_GENERIC_P (new_addr))
		    error ("conflicting named address spaces (generic vs %s) "
			   "for %q+D",
			   c_addr_space_name (old_addr), newdecl);
		  else if (ADDR_SPACE_GENERIC_P (old_addr))
		    error ("conflicting named address spaces (%s vs generic) "
			   "for %q+D",
			   c_addr_space_name (new_addr), newdecl);
		  else
		    error ("conflicting named address spaces (%s vs %s) "
			   "for %q+D",
			   c_addr_space_name (new_addr),
			   c_addr_space_name (old_addr),
			   newdecl);
		}

	      if (CLEAR_QUAL_ADDR_SPACE (new_quals)
		  != CLEAR_QUAL_ADDR_SPACE (old_quals))
		error ("conflicting type qualifiers for %q+D", newdecl);
	    }
	  else
	    error ("conflicting types for %q+D", newdecl);
	  diagnose_arglist_conflict (newdecl, olddecl, newtype, oldtype);
	  locate_old_decl (olddecl);
	  return false;
	}
    }

  /* Redeclaration of a type is a constraint violation (6.7.2.3p1),
     but silently ignore the redeclaration if either is in a system
     header.  (Conflicting redeclarations were handled above.)  This
     is allowed for C11 if the types are the same, not just
     compatible.  */
  if (TREE_CODE (newdecl) == TYPE_DECL)
    {
      bool types_different = false;
      int comptypes_result;

      comptypes_result
	= comptypes_check_different_types (oldtype, newtype, &types_different);

      if (comptypes_result != 1 || types_different)
	{
	  error ("redefinition of typedef %q+D with different type", newdecl);
	  locate_old_decl (olddecl);
	  return false;
	}

      if (DECL_IN_SYSTEM_HEADER (newdecl)
	  || DECL_IN_SYSTEM_HEADER (olddecl)
	  || TREE_NO_WARNING (newdecl)
	  || TREE_NO_WARNING (olddecl))
	return true;  /* Allow OLDDECL to continue in use.  */

      if (variably_modified_type_p (newtype, NULL))
	{
	  error ("redefinition of typedef %q+D with variably modified type",
		 newdecl);
	  locate_old_decl (olddecl);
	}
      else if (pedwarn_c99 (input_location, OPT_Wpedantic,
			    "redefinition of typedef %q+D", newdecl))
	locate_old_decl (olddecl);

      return true;
    }

  /* Function declarations can either be 'static' or 'extern' (no
     qualifier is equivalent to 'extern' - C99 6.2.2p5) and therefore
     can never conflict with each other on account of linkage
     (6.2.2p4).  Multiple definitions are not allowed (6.9p3,5) but
     gnu89 mode permits two definitions if one is 'extern inline' and
     one is not.  The non- extern-inline definition supersedes the
     extern-inline definition.  */

  else if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      /* If you declare a built-in function name as static, or
	 define the built-in with an old-style definition (so we
	 can't validate the argument list) the built-in definition is
	 overridden, but optionally warn this was a bad choice of name.  */
      if (DECL_BUILT_IN (olddecl)
	  && !C_DECL_DECLARED_BUILTIN (olddecl)
	  && (!TREE_PUBLIC (newdecl)
	      || (DECL_INITIAL (newdecl)
		  && !prototype_p (TREE_TYPE (newdecl)))))
	{
	  warning (OPT_Wshadow, "declaration of %q+D shadows "
		   "a built-in function", newdecl);
	  /* Discard the old built-in function.  */
	  return false;
	}

      if (DECL_INITIAL (newdecl))
	{
	  if (DECL_INITIAL (olddecl))
	    {
	      /* If both decls are in the same TU and the new declaration
		 isn't overriding an extern inline reject the new decl.
		 In c99, no overriding is allowed in the same translation
		 unit.  */
	      if ((!DECL_EXTERN_INLINE (olddecl)
		   || DECL_EXTERN_INLINE (newdecl)
		   || (!flag_gnu89_inline
		       && (!DECL_DECLARED_INLINE_P (olddecl)
			   || !lookup_attribute ("gnu_inline",
						 DECL_ATTRIBUTES (olddecl)))
		       && (!DECL_DECLARED_INLINE_P (newdecl)
			   || !lookup_attribute ("gnu_inline",
						 DECL_ATTRIBUTES (newdecl))))
		  )
		  && same_translation_unit_p (newdecl, olddecl))
		{
		  error ("redefinition of %q+D", newdecl);
		  locate_old_decl (olddecl);
		  return false;
		}
	    }
	}
      /* If we have a prototype after an old-style function definition,
	 the argument types must be checked specially.  */
      else if (DECL_INITIAL (olddecl)
	       && !prototype_p (oldtype) && prototype_p (newtype)
	       && TYPE_ACTUAL_ARG_TYPES (oldtype)
	       && !validate_proto_after_old_defn (newdecl, newtype, oldtype))
	{
	  locate_old_decl (olddecl);
	  return false;
	}
      /* A non-static declaration (even an "extern") followed by a
	 static declaration is undefined behavior per C99 6.2.2p3-5,7.
	 The same is true for a static forward declaration at block
	 scope followed by a non-static declaration/definition at file
	 scope.  Static followed by non-static at the same scope is
	 not undefined behavior, and is the most convenient way to get
	 some effects (see e.g.  what unwind-dw2-fde-glibc.c does to
	 the definition of _Unwind_Find_FDE in unwind-dw2-fde.c), but
	 we do diagnose it if -Wtraditional.  */
      if (TREE_PUBLIC (olddecl) && !TREE_PUBLIC (newdecl))
	{
	  /* Two exceptions to the rule.  If olddecl is an extern
	     inline, or a predeclared function that isn't actually
	     built in, newdecl silently overrides olddecl.  The latter
	     occur only in Objective C; see also above.  (FIXME: Make
	     Objective C use normal builtins.)  */
	  if (!DECL_IS_BUILTIN (olddecl)
	      && !DECL_EXTERN_INLINE (olddecl))
	    {
	      error ("static declaration of %q+D follows "
		     "non-static declaration", newdecl);
	      locate_old_decl (olddecl);
	    }
	  return false;
	}
      else if (TREE_PUBLIC (newdecl) && !TREE_PUBLIC (olddecl))
	{
	  if (DECL_CONTEXT (olddecl))
	    {
	      error ("non-static declaration of %q+D follows "
		     "static declaration", newdecl);
	      locate_old_decl (olddecl);
	      return false;
	    }
	  else if (warn_traditional)
	    {
	      warned |= warning (OPT_Wtraditional,
				 "non-static declaration of %q+D "
				 "follows static declaration", newdecl);
	    }
	}

      /* Make sure gnu_inline attribute is either not present, or
	 present on all inline decls.  */
      if (DECL_DECLARED_INLINE_P (olddecl)
	  && DECL_DECLARED_INLINE_P (newdecl))
	{
	  bool newa = lookup_attribute ("gnu_inline",
					DECL_ATTRIBUTES (newdecl)) != NULL;
	  bool olda = lookup_attribute ("gnu_inline",
					DECL_ATTRIBUTES (olddecl)) != NULL;
	  if (newa != olda)
	    {
	      error_at (input_location, "%<gnu_inline%> attribute present on %q+D",
			newa ? newdecl : olddecl);
	      error_at (DECL_SOURCE_LOCATION (newa ? olddecl : newdecl),
			"but not here");
	    }
	}
    }
  else if (VAR_P (newdecl))
    {
      /* Only variables can be thread-local, and all declarations must
	 agree on this property.  */
      if (C_DECL_THREADPRIVATE_P (olddecl) && !DECL_THREAD_LOCAL_P (newdecl))
	{
	  /* Nothing to check.  Since OLDDECL is marked threadprivate
	     and NEWDECL does not have a thread-local attribute, we
	     will merge the threadprivate attribute into NEWDECL.  */
	  ;
	}
      else if (DECL_THREAD_LOCAL_P (newdecl) != DECL_THREAD_LOCAL_P (olddecl))
	{
	  if (DECL_THREAD_LOCAL_P (newdecl))
	    error ("thread-local declaration of %q+D follows "
		   "non-thread-local declaration", newdecl);
	  else
	    error ("non-thread-local declaration of %q+D follows "
		   "thread-local declaration", newdecl);

	  locate_old_decl (olddecl);
	  return false;
	}

      /* Multiple initialized definitions are not allowed (6.9p3,5).  */
      if (DECL_INITIAL (newdecl) && DECL_INITIAL (olddecl))
	{
	  error ("redefinition of %q+D", newdecl);
	  locate_old_decl (olddecl);
	  return false;
	}

      /* Objects declared at file scope: if the first declaration had
	 external linkage (even if it was an external reference) the
	 second must have external linkage as well, or the behavior is
	 undefined.  If the first declaration had internal linkage, then
	 the second must too, or else be an external reference (in which
	 case the composite declaration still has internal linkage).
	 As for function declarations, we warn about the static-then-
	 extern case only for -Wtraditional.  See generally 6.2.2p3-5,7.  */
      if (DECL_FILE_SCOPE_P (newdecl)
	  && TREE_PUBLIC (newdecl) != TREE_PUBLIC (olddecl))
	{
	  if (DECL_EXTERNAL (newdecl))
	    {
	      if (!DECL_FILE_SCOPE_P (olddecl))
		{
		  error ("extern declaration of %q+D follows "
			 "declaration with no linkage", newdecl);
		  locate_old_decl (olddecl);
		  return false;
		}
	      else if (warn_traditional)
		{
		  warned |= warning (OPT_Wtraditional,
				     "non-static declaration of %q+D "
				     "follows static declaration", newdecl);
		}
	    }
	  else
	    {
	      if (TREE_PUBLIC (newdecl))
		error ("non-static declaration of %q+D follows "
		       "static declaration", newdecl);
	      else
		error ("static declaration of %q+D follows "
		       "non-static declaration", newdecl);

	      locate_old_decl (olddecl);
	      return false;
	    }
	}
      /* Two objects with the same name declared at the same block
	 scope must both be external references (6.7p3).  */
      else if (!DECL_FILE_SCOPE_P (newdecl))
	{
	  if (DECL_EXTERNAL (newdecl))
	    {
	      /* Extern with initializer at block scope, which will
		 already have received an error.  */
	    }
	  else if (DECL_EXTERNAL (olddecl))
	    {
	      error ("declaration of %q+D with no linkage follows "
		     "extern declaration", newdecl);
	      locate_old_decl (olddecl);
	    }
	  else
	    {
	      error ("redeclaration of %q+D with no linkage", newdecl);
	      locate_old_decl (olddecl);
	    }

	  return false;
	}

      /* C++ does not permit a decl to appear multiple times at file
	 scope.  */
      if (warn_cxx_compat
	  && DECL_FILE_SCOPE_P (newdecl)
	  && !DECL_EXTERNAL (newdecl)
	  && !DECL_EXTERNAL (olddecl))
	warned |= warning_at (DECL_SOURCE_LOCATION (newdecl),
			      OPT_Wc___compat,
			      ("duplicate declaration of %qD is "
			       "invalid in C++"),
			      newdecl);
    }

  /* warnings */
  /* All decls must agree on a visibility.  */
  if (CODE_CONTAINS_STRUCT (TREE_CODE (newdecl), TS_DECL_WITH_VIS)
      && DECL_VISIBILITY_SPECIFIED (newdecl) && DECL_VISIBILITY_SPECIFIED (olddecl)
      && DECL_VISIBILITY (newdecl) != DECL_VISIBILITY (olddecl))
    {
      warned |= warning (0, "redeclaration of %q+D with different visibility "
			 "(old visibility preserved)", newdecl);
    }

  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    warned |= diagnose_mismatched_attributes (olddecl, newdecl);
  else /* PARM_DECL, VAR_DECL */
    {
      /* Redeclaration of a parameter is a constraint violation (this is
	 not explicitly stated, but follows from C99 6.7p3 [no more than
	 one declaration of the same identifier with no linkage in the
	 same scope, except type tags] and 6.2.2p6 [parameters have no
	 linkage]).  We must check for a forward parameter declaration,
	 indicated by TREE_ASM_WRITTEN on the old declaration - this is
	 an extension, the mandatory diagnostic for which is handled by
	 mark_forward_parm_decls.  */

      if (TREE_CODE (newdecl) == PARM_DECL
	  && (!TREE_ASM_WRITTEN (olddecl) || TREE_ASM_WRITTEN (newdecl)))
	{
	  error ("redefinition of parameter %q+D", newdecl);
	  locate_old_decl (olddecl);
	  return false;
	}
    }

  /* Optional warning for completely redundant decls.  */
  if (!warned && !pedwarned
      && warn_redundant_decls
      /* Don't warn about a function declaration followed by a
	 definition.  */
      && !(TREE_CODE (newdecl) == FUNCTION_DECL
	   && DECL_INITIAL (newdecl) && !DECL_INITIAL (olddecl))
      /* Don't warn about redundant redeclarations of builtins.  */
      && !(TREE_CODE (newdecl) == FUNCTION_DECL
	   && !DECL_BUILT_IN (newdecl)
	   && DECL_BUILT_IN (olddecl)
	   && !C_DECL_DECLARED_BUILTIN (olddecl))
      /* Don't warn about an extern followed by a definition.  */
      && !(DECL_EXTERNAL (olddecl) && !DECL_EXTERNAL (newdecl))
      /* Don't warn about forward parameter decls.  */
      && !(TREE_CODE (newdecl) == PARM_DECL
	   && TREE_ASM_WRITTEN (olddecl) && !TREE_ASM_WRITTEN (newdecl))
      /* Don't warn about a variable definition following a declaration.  */
      && !(VAR_P (newdecl)
	   && DECL_INITIAL (newdecl) && !DECL_INITIAL (olddecl)))
    {
      warned = warning (OPT_Wredundant_decls, "redundant redeclaration of %q+D",
			newdecl);
    }

  /* Report location of previous decl/defn.  */
  if (warned || pedwarned)
    locate_old_decl (olddecl);

#undef DECL_EXTERN_INLINE

  return retval;
}

/* Subroutine of duplicate_decls.  NEWDECL has been found to be
   consistent with OLDDECL, but carries new information.  Merge the
   new information into OLDDECL.  This function issues no
   diagnostics.  */

static void
merge_decls (tree newdecl, tree olddecl, tree newtype, tree oldtype)
{
  bool new_is_definition = (TREE_CODE (newdecl) == FUNCTION_DECL
			    && DECL_INITIAL (newdecl) != NULL_TREE);
  bool new_is_prototype = (TREE_CODE (newdecl) == FUNCTION_DECL
			   && prototype_p (TREE_TYPE (newdecl)));
  bool old_is_prototype = (TREE_CODE (olddecl) == FUNCTION_DECL
			   && prototype_p (TREE_TYPE (olddecl)));

  /* For real parm decl following a forward decl, rechain the old decl
     in its new location and clear TREE_ASM_WRITTEN (it's not a
     forward decl anymore).  */
  if (TREE_CODE (newdecl) == PARM_DECL
      && TREE_ASM_WRITTEN (olddecl) && !TREE_ASM_WRITTEN (newdecl))
    {
      struct c_binding *b, **here;

      for (here = &current_scope->bindings; *here; here = &(*here)->prev)
	if ((*here)->decl == olddecl)
	  goto found;
      gcc_unreachable ();

    found:
      b = *here;
      *here = b->prev;
      b->prev = current_scope->bindings;
      current_scope->bindings = b;

      TREE_ASM_WRITTEN (olddecl) = 0;
    }

  DECL_ATTRIBUTES (newdecl)
    = targetm.merge_decl_attributes (olddecl, newdecl);

  /* For typedefs use the old type, as the new type's DECL_NAME points
     at newdecl, which will be ggc_freed.  */
  if (TREE_CODE (newdecl) == TYPE_DECL)
    {
      /* But NEWTYPE might have an attribute, honor that.  */
      tree tem = newtype;
      newtype = oldtype;

      if (TYPE_USER_ALIGN (tem))
	{
	  if (TYPE_ALIGN (tem) > TYPE_ALIGN (newtype))
	    SET_TYPE_ALIGN (newtype, TYPE_ALIGN (tem));
	  TYPE_USER_ALIGN (newtype) = true;
	}

      /* And remove the new type from the variants list.  */
      if (TYPE_NAME (TREE_TYPE (newdecl)) == newdecl)
	{
	  tree remove = TREE_TYPE (newdecl);
	  for (tree t = TYPE_MAIN_VARIANT (remove); ;
	       t = TYPE_NEXT_VARIANT (t))
	    if (TYPE_NEXT_VARIANT (t) == remove)
	      {
		TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (remove);
		break;
	      }
	}
    }

  /* Merge the data types specified in the two decls.  */
  TREE_TYPE (newdecl)
    = TREE_TYPE (olddecl)
    = composite_type (newtype, oldtype);

  /* Lay the type out, unless already done.  */
  if (!comptypes (oldtype, TREE_TYPE (newdecl)))
    {
      if (TREE_TYPE (newdecl) != error_mark_node)
	layout_type (TREE_TYPE (newdecl));
      if (TREE_CODE (newdecl) != FUNCTION_DECL
	  && TREE_CODE (newdecl) != TYPE_DECL
	  && TREE_CODE (newdecl) != CONST_DECL)
	layout_decl (newdecl, 0);
    }
  else
    {
      /* Since the type is OLDDECL's, make OLDDECL's size go with.  */
      DECL_SIZE (newdecl) = DECL_SIZE (olddecl);
      DECL_SIZE_UNIT (newdecl) = DECL_SIZE_UNIT (olddecl);
      SET_DECL_MODE (newdecl, DECL_MODE (olddecl));
      if (DECL_ALIGN (olddecl) > DECL_ALIGN (newdecl))
	{
	  SET_DECL_ALIGN (newdecl, DECL_ALIGN (olddecl));
	  DECL_USER_ALIGN (newdecl) |= DECL_USER_ALIGN (olddecl);
	}
      if (DECL_WARN_IF_NOT_ALIGN (olddecl)
	  > DECL_WARN_IF_NOT_ALIGN (newdecl))
	SET_DECL_WARN_IF_NOT_ALIGN (newdecl,
				    DECL_WARN_IF_NOT_ALIGN (olddecl));
    }

  /* Keep the old rtl since we can safely use it.  */
  if (HAS_RTL_P (olddecl))
    COPY_DECL_RTL (olddecl, newdecl);

  /* Merge the type qualifiers.  */
  if (TREE_READONLY (newdecl))
    TREE_READONLY (olddecl) = 1;

  if (TREE_THIS_VOLATILE (newdecl))
    TREE_THIS_VOLATILE (olddecl) = 1;

  /* Merge deprecatedness.  */
  if (TREE_DEPRECATED (newdecl))
    TREE_DEPRECATED (olddecl) = 1;

  /* If a decl is in a system header and the other isn't, keep the one on the
     system header. Otherwise, keep source location of definition rather than
     declaration and of prototype rather than non-prototype unless that
     prototype is built-in.  */
  if (CODE_CONTAINS_STRUCT (TREE_CODE (olddecl), TS_DECL_WITH_VIS)
      && DECL_IN_SYSTEM_HEADER (olddecl)
      && !DECL_IN_SYSTEM_HEADER (newdecl) )
    DECL_SOURCE_LOCATION (newdecl) = DECL_SOURCE_LOCATION (olddecl);
  else if (CODE_CONTAINS_STRUCT (TREE_CODE (olddecl), TS_DECL_WITH_VIS)
	   && DECL_IN_SYSTEM_HEADER (newdecl)
	   && !DECL_IN_SYSTEM_HEADER (olddecl))
    DECL_SOURCE_LOCATION (olddecl) = DECL_SOURCE_LOCATION (newdecl);
  else if ((DECL_INITIAL (newdecl) == NULL_TREE
	    && DECL_INITIAL (olddecl) != NULL_TREE)
	   || (old_is_prototype && !new_is_prototype
	       && !C_DECL_BUILTIN_PROTOTYPE (olddecl)))
    DECL_SOURCE_LOCATION (newdecl) = DECL_SOURCE_LOCATION (olddecl);

  /* Merge the initialization information.  */
   if (DECL_INITIAL (newdecl) == NULL_TREE)
    DECL_INITIAL (newdecl) = DECL_INITIAL (olddecl);

  /* Merge the threadprivate attribute.  */
  if (VAR_P (olddecl) && C_DECL_THREADPRIVATE_P (olddecl))
    C_DECL_THREADPRIVATE_P (newdecl) = 1;

  if (CODE_CONTAINS_STRUCT (TREE_CODE (olddecl), TS_DECL_WITH_VIS))
    {
      /* Copy the assembler name.
	 Currently, it can only be defined in the prototype.  */
      COPY_DECL_ASSEMBLER_NAME (olddecl, newdecl);

      /* Use visibility of whichever declaration had it specified */
      if (DECL_VISIBILITY_SPECIFIED (olddecl))
	{
	  DECL_VISIBILITY (newdecl) = DECL_VISIBILITY (olddecl);
	  DECL_VISIBILITY_SPECIFIED (newdecl) = 1;
	}

      if (TREE_CODE (newdecl) == FUNCTION_DECL)
	{
	  DECL_STATIC_CONSTRUCTOR(newdecl) |= DECL_STATIC_CONSTRUCTOR(olddecl);
	  DECL_STATIC_DESTRUCTOR (newdecl) |= DECL_STATIC_DESTRUCTOR (olddecl);
	  DECL_NO_LIMIT_STACK (newdecl) |= DECL_NO_LIMIT_STACK (olddecl);
	  DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (newdecl)
	    |= DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (olddecl);
	  TREE_THIS_VOLATILE (newdecl) |= TREE_THIS_VOLATILE (olddecl);
	  DECL_IS_MALLOC (newdecl) |= DECL_IS_MALLOC (olddecl);
	  DECL_IS_OPERATOR_NEW (newdecl) |= DECL_IS_OPERATOR_NEW (olddecl);
	  TREE_READONLY (newdecl) |= TREE_READONLY (olddecl);
	  DECL_PURE_P (newdecl) |= DECL_PURE_P (olddecl);
	  DECL_IS_NOVOPS (newdecl) |= DECL_IS_NOVOPS (olddecl);
	}

      /* Merge the storage class information.  */
      merge_weak (newdecl, olddecl);

      /* For functions, static overrides non-static.  */
      if (TREE_CODE (newdecl) == FUNCTION_DECL)
	{
	  TREE_PUBLIC (newdecl) &= TREE_PUBLIC (olddecl);
	  /* This is since we don't automatically
	     copy the attributes of NEWDECL into OLDDECL.  */
	  TREE_PUBLIC (olddecl) = TREE_PUBLIC (newdecl);
	  /* If this clears `static', clear it in the identifier too.  */
	  if (!TREE_PUBLIC (olddecl))
	    TREE_PUBLIC (DECL_NAME (olddecl)) = 0;
	}
    }

  /* In c99, 'extern' declaration before (or after) 'inline' means this
     function is not DECL_EXTERNAL, unless 'gnu_inline' attribute
     is present.  */
  if (TREE_CODE (newdecl) == FUNCTION_DECL
      && !flag_gnu89_inline
      && (DECL_DECLARED_INLINE_P (newdecl)
	  || DECL_DECLARED_INLINE_P (olddecl))
      && (!DECL_DECLARED_INLINE_P (newdecl)
	  || !DECL_DECLARED_INLINE_P (olddecl)
	  || !DECL_EXTERNAL (olddecl))
      && DECL_EXTERNAL (newdecl)
      && !lookup_attribute ("gnu_inline", DECL_ATTRIBUTES (newdecl))
      && !current_function_decl)
    DECL_EXTERNAL (newdecl) = 0;

  /* An inline definition following a static declaration is not
     DECL_EXTERNAL.  */
  if (new_is_definition
      && (DECL_DECLARED_INLINE_P (newdecl)
	  || DECL_DECLARED_INLINE_P (olddecl))
      && !TREE_PUBLIC (olddecl))
    DECL_EXTERNAL (newdecl) = 0;

  if (DECL_EXTERNAL (newdecl))
    {
      TREE_STATIC (newdecl) = TREE_STATIC (olddecl);
      DECL_EXTERNAL (newdecl) = DECL_EXTERNAL (olddecl);

      /* An extern decl does not override previous storage class.  */
      TREE_PUBLIC (newdecl) = TREE_PUBLIC (olddecl);
      if (!DECL_EXTERNAL (newdecl))
	{
	  DECL_CONTEXT (newdecl) = DECL_CONTEXT (olddecl);
	  DECL_COMMON (newdecl) = DECL_COMMON (olddecl);
	}
    }
  else
    {
      TREE_STATIC (olddecl) = TREE_STATIC (newdecl);
      TREE_PUBLIC (olddecl) = TREE_PUBLIC (newdecl);
    }

  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      /* If we're redefining a function previously defined as extern
	 inline, make sure we emit debug info for the inline before we
	 throw it away, in case it was inlined into a function that
	 hasn't been written out yet.  */
      if (new_is_definition && DECL_INITIAL (olddecl))
	/* The new defn must not be inline.  */
	DECL_UNINLINABLE (newdecl) = 1;
      else
	{
	  /* If either decl says `inline', this fn is inline, unless
	     its definition was passed already.  */
	  if (DECL_DECLARED_INLINE_P (newdecl)
	      || DECL_DECLARED_INLINE_P (olddecl))
	    DECL_DECLARED_INLINE_P (newdecl) = 1;

	  DECL_UNINLINABLE (newdecl) = DECL_UNINLINABLE (olddecl)
	    = (DECL_UNINLINABLE (newdecl) || DECL_UNINLINABLE (olddecl));

	  DECL_DISREGARD_INLINE_LIMITS (newdecl)
	    = DECL_DISREGARD_INLINE_LIMITS (olddecl)
	    = (DECL_DISREGARD_INLINE_LIMITS (newdecl)
	       || DECL_DISREGARD_INLINE_LIMITS (olddecl));
	}

      if (DECL_BUILT_IN (olddecl))
	{
	  /* If redeclaring a builtin function, it stays built in.
	     But it gets tagged as having been declared.  */
	  DECL_BUILT_IN_CLASS (newdecl) = DECL_BUILT_IN_CLASS (olddecl);
	  DECL_FUNCTION_CODE (newdecl) = DECL_FUNCTION_CODE (olddecl);
	  C_DECL_DECLARED_BUILTIN (newdecl) = 1;
	  if (new_is_prototype)
	    {
	      C_DECL_BUILTIN_PROTOTYPE (newdecl) = 0;
	      if (DECL_BUILT_IN_CLASS (newdecl) == BUILT_IN_NORMAL)
		{
		  enum built_in_function fncode = DECL_FUNCTION_CODE (newdecl);
		  switch (fncode)
		    {
		      /* If a compatible prototype of these builtin functions
			 is seen, assume the runtime implements it with the
			 expected semantics.  */
		    case BUILT_IN_STPCPY:
		      if (builtin_decl_explicit_p (fncode))
			set_builtin_decl_implicit_p (fncode, true);
		      break;
		    default:
		      if (builtin_decl_explicit_p (fncode))
			set_builtin_decl_declared_p (fncode, true);
		      break;
		    }

		  copy_attributes_to_builtin (newdecl);
		}
	    }
	  else
	    C_DECL_BUILTIN_PROTOTYPE (newdecl)
	      = C_DECL_BUILTIN_PROTOTYPE (olddecl);
	}

      /* Preserve function specific target and optimization options */
      if (DECL_FUNCTION_SPECIFIC_TARGET (olddecl)
	  && !DECL_FUNCTION_SPECIFIC_TARGET (newdecl))
	DECL_FUNCTION_SPECIFIC_TARGET (newdecl)
	  = DECL_FUNCTION_SPECIFIC_TARGET (olddecl);

      if (DECL_FUNCTION_SPECIFIC_OPTIMIZATION (olddecl)
	  && !DECL_FUNCTION_SPECIFIC_OPTIMIZATION (newdecl))
	DECL_FUNCTION_SPECIFIC_OPTIMIZATION (newdecl)
	  = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (olddecl);

      /* Also preserve various other info from the definition.  */
      if (!new_is_definition)
	{
	  tree t;
	  DECL_RESULT (newdecl) = DECL_RESULT (olddecl);
	  DECL_INITIAL (newdecl) = DECL_INITIAL (olddecl);
	  DECL_STRUCT_FUNCTION (newdecl) = DECL_STRUCT_FUNCTION (olddecl);
	  DECL_SAVED_TREE (newdecl) = DECL_SAVED_TREE (olddecl);
	  DECL_ARGUMENTS (newdecl) = copy_list (DECL_ARGUMENTS (olddecl));
	  for (t = DECL_ARGUMENTS (newdecl); t ; t = DECL_CHAIN (t))
	    DECL_CONTEXT (t) = newdecl;

	  /* See if we've got a function to instantiate from.  */
	  if (DECL_SAVED_TREE (olddecl))
	    DECL_ABSTRACT_ORIGIN (newdecl)
	      = DECL_ABSTRACT_ORIGIN (olddecl);
	}
    }

  /* Merge the USED information.  */
  if (TREE_USED (olddecl))
    TREE_USED (newdecl) = 1;
  else if (TREE_USED (newdecl))
    TREE_USED (olddecl) = 1;
  if (VAR_P (olddecl) || TREE_CODE (olddecl) == PARM_DECL)
    DECL_READ_P (newdecl) |= DECL_READ_P (olddecl);
  if (DECL_PRESERVE_P (olddecl))
    DECL_PRESERVE_P (newdecl) = 1;
  else if (DECL_PRESERVE_P (newdecl))
    DECL_PRESERVE_P (olddecl) = 1;

  /* Merge DECL_COMMON */
  if (VAR_P (olddecl) && VAR_P (newdecl)
      && !lookup_attribute ("common", DECL_ATTRIBUTES (newdecl))
      && !lookup_attribute ("nocommon", DECL_ATTRIBUTES (newdecl)))
    DECL_COMMON (newdecl) = DECL_COMMON (newdecl) && DECL_COMMON (olddecl);

  /* Copy most of the decl-specific fields of NEWDECL into OLDDECL.
     But preserve OLDDECL's DECL_UID, DECL_CONTEXT and
     DECL_ARGUMENTS (if appropriate).  */
  {
    unsigned olddecl_uid = DECL_UID (olddecl);
    tree olddecl_context = DECL_CONTEXT (olddecl);
    tree olddecl_arguments = NULL;
    if (TREE_CODE (olddecl) == FUNCTION_DECL)
      olddecl_arguments = DECL_ARGUMENTS (olddecl);

    memcpy ((char *) olddecl + sizeof (struct tree_common),
	    (char *) newdecl + sizeof (struct tree_common),
	    sizeof (struct tree_decl_common) - sizeof (struct tree_common));
    DECL_USER_ALIGN (olddecl) = DECL_USER_ALIGN (newdecl);
    switch (TREE_CODE (olddecl))
      {
      case FUNCTION_DECL:
      case VAR_DECL:
	{
	  struct symtab_node *snode = olddecl->decl_with_vis.symtab_node;

	  memcpy ((char *) olddecl + sizeof (struct tree_decl_common),
		  (char *) newdecl + sizeof (struct tree_decl_common),
		  tree_code_size (TREE_CODE (olddecl)) - sizeof (struct tree_decl_common));
	  olddecl->decl_with_vis.symtab_node = snode;

	  if ((DECL_EXTERNAL (olddecl)
	       || TREE_PUBLIC (olddecl)
	       || TREE_STATIC (olddecl))
	      && DECL_SECTION_NAME (newdecl) != NULL)
	    set_decl_section_name (olddecl, DECL_SECTION_NAME (newdecl));

	  /* This isn't quite correct for something like
		int __thread x attribute ((tls_model ("local-exec")));
		extern int __thread x;
	     as we'll lose the "local-exec" model.  */
	  if (VAR_P (olddecl) && DECL_THREAD_LOCAL_P (newdecl))
	    set_decl_tls_model (olddecl, DECL_TLS_MODEL (newdecl));
	  break;
	}

      case FIELD_DECL:
      case PARM_DECL:
      case LABEL_DECL:
      case RESULT_DECL:
      case CONST_DECL:
      case TYPE_DECL:
	memcpy ((char *) olddecl + sizeof (struct tree_decl_common),
		(char *) newdecl + sizeof (struct tree_decl_common),
		tree_code_size (TREE_CODE (olddecl)) - sizeof (struct tree_decl_common));
	break;

      default:

	memcpy ((char *) olddecl + sizeof (struct tree_decl_common),
		(char *) newdecl + sizeof (struct tree_decl_common),
		sizeof (struct tree_decl_non_common) - sizeof (struct tree_decl_common));
      }
    DECL_UID (olddecl) = olddecl_uid;
    DECL_CONTEXT (olddecl) = olddecl_context;
    if (TREE_CODE (olddecl) == FUNCTION_DECL)
      DECL_ARGUMENTS (olddecl) = olddecl_arguments;
  }

  /* If OLDDECL had its DECL_RTL instantiated, re-invoke make_decl_rtl
     so that encode_section_info has a chance to look at the new decl
     flags and attributes.  */
  if (DECL_RTL_SET_P (olddecl)
      && (TREE_CODE (olddecl) == FUNCTION_DECL
	  || (VAR_P (olddecl) && TREE_STATIC (olddecl))))
    make_decl_rtl (olddecl);
}

/* Handle when a new declaration NEWDECL has the same name as an old
   one OLDDECL in the same binding contour.  Prints an error message
   if appropriate.

   If safely possible, alter OLDDECL to look like NEWDECL, and return
   true.  Otherwise, return false.  */

static bool
duplicate_decls (tree newdecl, tree olddecl)
{
  tree newtype = NULL, oldtype = NULL;

  if (!diagnose_mismatched_decls (newdecl, olddecl, &newtype, &oldtype))
    {
      /* Avoid `unused variable' and other warnings for OLDDECL.  */
      TREE_NO_WARNING (olddecl) = 1;
      return false;
    }

  merge_decls (newdecl, olddecl, newtype, oldtype);

  /* The NEWDECL will no longer be needed.

     Before releasing the node, be sure to remove function from symbol
     table that might have been inserted there to record comdat group.
     Be sure to however do not free DECL_STRUCT_FUNCTION because this
     structure is shared in between NEWDECL and OLDECL.  */
  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    DECL_STRUCT_FUNCTION (newdecl) = NULL;
  if (VAR_OR_FUNCTION_DECL_P (newdecl))
    {
      struct symtab_node *snode = symtab_node::get (newdecl);
      if (snode)
	snode->remove ();
    }
  ggc_free (newdecl);
  return true;
}


/* Check whether decl-node NEW_DECL shadows an existing declaration.  */
static void
warn_if_shadowing (tree new_decl)
{
  struct c_binding *b;

  /* Shadow warnings wanted?  */
  if (!(warn_shadow
        || warn_shadow_local
        || warn_shadow_compatible_local)
      /* No shadow warnings for internally generated vars.  */
      || DECL_IS_BUILTIN (new_decl)
      /* No shadow warnings for vars made for inlining.  */
      || DECL_FROM_INLINE (new_decl))
    return;

  /* Is anything being shadowed?  Invisible decls do not count.  */
  for (b = I_SYMBOL_BINDING (DECL_NAME (new_decl)); b; b = b->shadowed)
    if (b->decl && b->decl != new_decl && !b->invisible
	&& (b->decl == error_mark_node
	    || diagnostic_report_warnings_p (global_dc,
					     DECL_SOURCE_LOCATION (b->decl))))
      {
	tree old_decl = b->decl;
	bool warned = false;

	if (old_decl == error_mark_node)
	  {
	    warning (OPT_Wshadow, "declaration of %q+D shadows previous "
		     "non-variable", new_decl);
	    break;
	  }
	else if (TREE_CODE (old_decl) == PARM_DECL)
	  {
	    enum opt_code warning_code;

	    /* If '-Wshadow=compatible-local' is specified without other
	       -Wshadow= flags, we will warn only when the types of the
	       shadowing variable (i.e. new_decl) and the shadowed variable
	       (old_decl) are compatible.  */
	    if (warn_shadow)
	      warning_code = OPT_Wshadow;
	    else if (comptypes (TREE_TYPE (old_decl), TREE_TYPE (new_decl)))
	      warning_code = OPT_Wshadow_compatible_local;
	    else
	      warning_code = OPT_Wshadow_local;
	    warned = warning_at (DECL_SOURCE_LOCATION (new_decl), warning_code,
				 "declaration of %qD shadows a parameter",
				 new_decl);
	  }
	else if (DECL_FILE_SCOPE_P (old_decl))
	  {
	    /* Do not warn if a variable shadows a function, unless
	       the variable is a function or a pointer-to-function.  */
	    if (TREE_CODE (old_decl) == FUNCTION_DECL
		&& TREE_CODE (new_decl) != FUNCTION_DECL
		&& !FUNCTION_POINTER_TYPE_P (TREE_TYPE (new_decl)))
		continue;

	    warned = warning_at (DECL_SOURCE_LOCATION (new_decl), OPT_Wshadow,
				 "declaration of %qD shadows a global "
				 "declaration",
				 new_decl);
	  }
	else if (TREE_CODE (old_decl) == FUNCTION_DECL
		 && DECL_BUILT_IN (old_decl))
	  {
	    warning (OPT_Wshadow, "declaration of %q+D shadows "
		     "a built-in function", new_decl);
	    break;
	  }
	else
	  {
	    enum opt_code warning_code;

	    /* If '-Wshadow=compatible-local' is specified without other
	       -Wshadow= flags, we will warn only when the types of the
	       shadowing variable (i.e. new_decl) and the shadowed variable
	       (old_decl) are compatible.  */
	    if (warn_shadow)
	      warning_code = OPT_Wshadow;
	    else if (comptypes (TREE_TYPE (old_decl), TREE_TYPE (new_decl)))
	      warning_code = OPT_Wshadow_compatible_local;
	    else
	      warning_code = OPT_Wshadow_local;
	    warned = warning_at (DECL_SOURCE_LOCATION (new_decl), warning_code,
				 "declaration of %qD shadows a previous local",
				 new_decl);
	  }

	if (warned)
	  inform (DECL_SOURCE_LOCATION (old_decl),
		  "shadowed declaration is here");

	break;
      }
}

/* Record a decl-node X as belonging to the current lexical scope.
   Check for errors (such as an incompatible declaration for the same
   name already seen in the same scope).

   Returns either X or an old decl for the same name.
   If an old decl is returned, it may have been smashed
   to agree with what X says.  */

tree
pushdecl (tree x)
{
  tree name = DECL_NAME (x);
  struct c_scope *scope = current_scope;
  struct c_binding *b;
  bool nested = false;
  location_t locus = DECL_SOURCE_LOCATION (x);

  /* Must set DECL_CONTEXT for everything not at file scope or
     DECL_FILE_SCOPE_P won't work.  Local externs don't count
     unless they have initializers (which generate code).  */
  if (current_function_decl
      && (!VAR_OR_FUNCTION_DECL_P (x)
	  || DECL_INITIAL (x) || !DECL_EXTERNAL (x)))
    DECL_CONTEXT (x) = current_function_decl;

  /* Anonymous decls are just inserted in the scope.  */
  if (!name)
    {
      bind (name, x, scope, /*invisible=*/false, /*nested=*/false,
	    locus);
      return x;
    }

  /* First, see if there is another declaration with the same name in
     the current scope.  If there is, duplicate_decls may do all the
     work for us.  If duplicate_decls returns false, that indicates
     two incompatible decls in the same scope; we are to silently
     replace the old one (duplicate_decls has issued all appropriate
     diagnostics).  In particular, we should not consider possible
     duplicates in the external scope, or shadowing.  */
  b = I_SYMBOL_BINDING (name);
  if (b && B_IN_SCOPE (b, scope))
    {
      struct c_binding *b_ext, *b_use;
      tree type = TREE_TYPE (x);
      tree visdecl = b->decl;
      tree vistype = TREE_TYPE (visdecl);
      if (TREE_CODE (TREE_TYPE (x)) == ARRAY_TYPE
	  && COMPLETE_TYPE_P (TREE_TYPE (x)))
	b->inner_comp = false;
      b_use = b;
      b_ext = b;
      /* If this is an external linkage declaration, we should check
	 for compatibility with the type in the external scope before
	 setting the type at this scope based on the visible
	 information only.  */
      if (TREE_PUBLIC (x) && TREE_PUBLIC (visdecl))
	{
	  while (b_ext && !B_IN_EXTERNAL_SCOPE (b_ext))
	    b_ext = b_ext->shadowed;
	  if (b_ext)
	    {
	      b_use = b_ext;
	      if (b_use->u.type)
		TREE_TYPE (b_use->decl) = b_use->u.type;
	    }
	}
      if (duplicate_decls (x, b_use->decl))
	{
	  if (b_use != b)
	    {
	      /* Save the updated type in the external scope and
		 restore the proper type for this scope.  */
	      tree thistype;
	      if (comptypes (vistype, type))
		thistype = composite_type (vistype, type);
	      else
		thistype = TREE_TYPE (b_use->decl);
	      b_use->u.type = TREE_TYPE (b_use->decl);
	      if (TREE_CODE (b_use->decl) == FUNCTION_DECL
		  && DECL_BUILT_IN (b_use->decl))
		thistype
		  = build_type_attribute_variant (thistype,
						  TYPE_ATTRIBUTES
						  (b_use->u.type));
	      TREE_TYPE (b_use->decl) = thistype;
	    }
	  return b_use->decl;
	}
      else
	goto skip_external_and_shadow_checks;
    }

  /* All declarations with external linkage, and all external
     references, go in the external scope, no matter what scope is
     current.  However, the binding in that scope is ignored for
     purposes of normal name lookup.  A separate binding structure is
     created in the requested scope; this governs the normal
     visibility of the symbol.

     The binding in the externals scope is used exclusively for
     detecting duplicate declarations of the same object, no matter
     what scope they are in; this is what we do here.  (C99 6.2.7p2:
     All declarations that refer to the same object or function shall
     have compatible type; otherwise, the behavior is undefined.)  */
  if (DECL_EXTERNAL (x) || scope == file_scope)
    {
      tree type = TREE_TYPE (x);
      tree vistype = NULL_TREE;
      tree visdecl = NULL_TREE;
      bool type_saved = false;
      if (b && !B_IN_EXTERNAL_SCOPE (b)
	  && VAR_OR_FUNCTION_DECL_P (b->decl)
	  && DECL_FILE_SCOPE_P (b->decl))
	{
	  visdecl = b->decl;
	  vistype = TREE_TYPE (visdecl);
	}
      if (scope != file_scope
	  && !DECL_IN_SYSTEM_HEADER (x))
	warning_at (locus, OPT_Wnested_externs,
		    "nested extern declaration of %qD", x);

      while (b && !B_IN_EXTERNAL_SCOPE (b))
	{
	  /* If this decl might be modified, save its type.  This is
	     done here rather than when the decl is first bound
	     because the type may change after first binding, through
	     being completed or through attributes being added.  If we
	     encounter multiple such decls, only the first should have
	     its type saved; the others will already have had their
	     proper types saved and the types will not have changed as
	     their scopes will not have been re-entered.  */
	  if (DECL_P (b->decl) && DECL_FILE_SCOPE_P (b->decl) && !type_saved)
	    {
	      b->u.type = TREE_TYPE (b->decl);
	      type_saved = true;
	    }
	  if (B_IN_FILE_SCOPE (b)
	      && VAR_P (b->decl)
	      && TREE_STATIC (b->decl)
	      && TREE_CODE (TREE_TYPE (b->decl)) == ARRAY_TYPE
	      && !TYPE_DOMAIN (TREE_TYPE (b->decl))
	      && TREE_CODE (type) == ARRAY_TYPE
	      && TYPE_DOMAIN (type)
	      && TYPE_MAX_VALUE (TYPE_DOMAIN (type))
	      && !integer_zerop (TYPE_MAX_VALUE (TYPE_DOMAIN (type))))
	    {
	      /* Array type completed in inner scope, which should be
		 diagnosed if the completion does not have size 1 and
		 it does not get completed in the file scope.  */
	      b->inner_comp = true;
	    }
	  b = b->shadowed;
	}

      /* If a matching external declaration has been found, set its
	 type to the composite of all the types of that declaration.
	 After the consistency checks, it will be reset to the
	 composite of the visible types only.  */
      if (b && (TREE_PUBLIC (x) || same_translation_unit_p (x, b->decl))
	  && b->u.type)
	TREE_TYPE (b->decl) = b->u.type;

      /* The point of the same_translation_unit_p check here is,
	 we want to detect a duplicate decl for a construct like
	 foo() { extern bar(); } ... static bar();  but not if
	 they are in different translation units.  In any case,
	 the static does not go in the externals scope.  */
      if (b
	  && (TREE_PUBLIC (x) || same_translation_unit_p (x, b->decl))
	  && duplicate_decls (x, b->decl))
	{
	  tree thistype;
	  if (vistype)
	    {
	      if (comptypes (vistype, type))
		thistype = composite_type (vistype, type);
	      else
		thistype = TREE_TYPE (b->decl);
	    }
	  else
	    thistype = type;
	  b->u.type = TREE_TYPE (b->decl);
	  if (TREE_CODE (b->decl) == FUNCTION_DECL && DECL_BUILT_IN (b->decl))
	    thistype
	      = build_type_attribute_variant (thistype,
					      TYPE_ATTRIBUTES (b->u.type));
	  TREE_TYPE (b->decl) = thistype;
	  bind (name, b->decl, scope, /*invisible=*/false, /*nested=*/true,
		locus);
	  return b->decl;
	}
      else if (TREE_PUBLIC (x))
	{
	  if (visdecl && !b && duplicate_decls (x, visdecl))
	    {
	      /* An external declaration at block scope referring to a
		 visible entity with internal linkage.  The composite
		 type will already be correct for this scope, so we
		 just need to fall through to make the declaration in
		 this scope.  */
	      nested = true;
	      x = visdecl;
	    }
	  else
	    {
	      bind (name, x, external_scope, /*invisible=*/true,
		    /*nested=*/false, locus);
	      nested = true;
	    }
	}
    }

  if (TREE_CODE (x) != PARM_DECL)
    warn_if_shadowing (x);

 skip_external_and_shadow_checks:
  if (TREE_CODE (x) == TYPE_DECL)
    {
      /* So this is a typedef, set its underlying type.  */
      set_underlying_type (x);

      /* If X is a typedef defined in the current function, record it
	 for the purpose of implementing the -Wunused-local-typedefs
	 warning.  */
      record_locally_defined_typedef (x);
    }

  bind (name, x, scope, /*invisible=*/false, nested, locus);

  /* If x's type is incomplete because it's based on a
     structure or union which has not yet been fully declared,
     attach it to that structure or union type, so we can go
     back and complete the variable declaration later, if the
     structure or union gets fully declared.

     If the input is erroneous, we can have error_mark in the type
     slot (e.g. "f(void a, ...)") - that doesn't count as an
     incomplete type.  */
  if (TREE_TYPE (x) != error_mark_node
      && !COMPLETE_TYPE_P (TREE_TYPE (x)))
    {
      tree element = TREE_TYPE (x);

      while (TREE_CODE (element) == ARRAY_TYPE)
	element = TREE_TYPE (element);
      element = TYPE_MAIN_VARIANT (element);

      if (RECORD_OR_UNION_TYPE_P (element)
	  && (TREE_CODE (x) != TYPE_DECL
	      || TREE_CODE (TREE_TYPE (x)) == ARRAY_TYPE)
	  && !COMPLETE_TYPE_P (element))
	C_TYPE_INCOMPLETE_VARS (element)
	  = tree_cons (NULL_TREE, x, C_TYPE_INCOMPLETE_VARS (element));
    }
  return x;
}


/* Issue a warning about implicit function declaration.  ID is the function
   identifier, OLDDECL is a declaration of the function in a different scope,
   or NULL_TREE.  */

static void
implicit_decl_warning (location_t loc, tree id, tree olddecl)
{
  if (!warn_implicit_function_declaration)
    return;

  bool warned;
  const char *hint = NULL;
  if (!olddecl)
    hint = lookup_name_fuzzy (id, FUZZY_LOOKUP_FUNCTION_NAME);

  if (flag_isoc99)
    {
      if (hint)
	{
	  gcc_rich_location richloc (loc);
	  richloc.add_fixit_replace (hint);
	  warned = pedwarn_at_rich_loc
	    (&richloc, OPT_Wimplicit_function_declaration,
	     "implicit declaration of function %qE; did you mean %qs?",
	     id, hint);
	}
      else
	warned = pedwarn (loc, OPT_Wimplicit_function_declaration,
			  "implicit declaration of function %qE", id);
    }
  else if (hint)
    {
      gcc_rich_location richloc (loc);
      richloc.add_fixit_replace (hint);
      warned = warning_at_rich_loc
	(&richloc, OPT_Wimplicit_function_declaration,
	 G_("implicit declaration of function %qE; did you mean %qs?"),
	 id, hint);
    }
  else
    warned = warning_at (loc, OPT_Wimplicit_function_declaration,
			 G_("implicit declaration of function %qE"), id);

  if (olddecl && warned)
    locate_old_decl (olddecl);
}

/* This function represents mapping of a function code FCODE
   to its respective header.  */

static const char *
header_for_builtin_fn (enum built_in_function fcode)
{
  switch (fcode)
    {
    CASE_FLT_FN (BUILT_IN_ACOS):
    CASE_FLT_FN (BUILT_IN_ACOSH):
    CASE_FLT_FN (BUILT_IN_ASIN):
    CASE_FLT_FN (BUILT_IN_ASINH):
    CASE_FLT_FN (BUILT_IN_ATAN):
    CASE_FLT_FN (BUILT_IN_ATANH):
    CASE_FLT_FN (BUILT_IN_ATAN2):
    CASE_FLT_FN (BUILT_IN_CBRT):
    CASE_FLT_FN (BUILT_IN_CEIL):
    CASE_FLT_FN (BUILT_IN_COPYSIGN):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_COPYSIGN):
    CASE_FLT_FN (BUILT_IN_COS):
    CASE_FLT_FN (BUILT_IN_COSH):
    CASE_FLT_FN (BUILT_IN_ERF):
    CASE_FLT_FN (BUILT_IN_ERFC):
    CASE_FLT_FN (BUILT_IN_EXP):
    CASE_FLT_FN (BUILT_IN_EXP2):
    CASE_FLT_FN (BUILT_IN_EXPM1):
    CASE_FLT_FN (BUILT_IN_FABS):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_FABS):
    CASE_FLT_FN (BUILT_IN_FDIM):
    CASE_FLT_FN (BUILT_IN_FLOOR):
    CASE_FLT_FN (BUILT_IN_FMA):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_FMA):
    CASE_FLT_FN (BUILT_IN_FMAX):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_FMAX):
    CASE_FLT_FN (BUILT_IN_FMIN):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_FMIN):
    CASE_FLT_FN (BUILT_IN_FMOD):
    CASE_FLT_FN (BUILT_IN_FREXP):
    CASE_FLT_FN (BUILT_IN_HYPOT):
    CASE_FLT_FN (BUILT_IN_ILOGB):
    CASE_FLT_FN (BUILT_IN_LDEXP):
    CASE_FLT_FN (BUILT_IN_LGAMMA):
    CASE_FLT_FN (BUILT_IN_LLRINT):
    CASE_FLT_FN (BUILT_IN_LLROUND):
    CASE_FLT_FN (BUILT_IN_LOG):
    CASE_FLT_FN (BUILT_IN_LOG10):
    CASE_FLT_FN (BUILT_IN_LOG1P):
    CASE_FLT_FN (BUILT_IN_LOG2):
    CASE_FLT_FN (BUILT_IN_LOGB):
    CASE_FLT_FN (BUILT_IN_LRINT):
    CASE_FLT_FN (BUILT_IN_LROUND):
    CASE_FLT_FN (BUILT_IN_MODF):
    CASE_FLT_FN (BUILT_IN_NAN):
    CASE_FLT_FN (BUILT_IN_NEARBYINT):
    CASE_FLT_FN (BUILT_IN_NEXTAFTER):
    CASE_FLT_FN (BUILT_IN_NEXTTOWARD):
    CASE_FLT_FN (BUILT_IN_POW):
    CASE_FLT_FN (BUILT_IN_REMAINDER):
    CASE_FLT_FN (BUILT_IN_REMQUO):
    CASE_FLT_FN (BUILT_IN_RINT):
    CASE_FLT_FN (BUILT_IN_ROUND):
    CASE_FLT_FN (BUILT_IN_SCALBLN):
    CASE_FLT_FN (BUILT_IN_SCALBN):
    CASE_FLT_FN (BUILT_IN_SIN):
    CASE_FLT_FN (BUILT_IN_SINH):
    CASE_FLT_FN (BUILT_IN_SINCOS):
    CASE_FLT_FN (BUILT_IN_SQRT):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_SQRT):
    CASE_FLT_FN (BUILT_IN_TAN):
    CASE_FLT_FN (BUILT_IN_TANH):
    CASE_FLT_FN (BUILT_IN_TGAMMA):
    CASE_FLT_FN (BUILT_IN_TRUNC):
    case BUILT_IN_ISINF:
    case BUILT_IN_ISNAN:
      return "<math.h>";
    CASE_FLT_FN (BUILT_IN_CABS):
    CASE_FLT_FN (BUILT_IN_CACOS):
    CASE_FLT_FN (BUILT_IN_CACOSH):
    CASE_FLT_FN (BUILT_IN_CARG):
    CASE_FLT_FN (BUILT_IN_CASIN):
    CASE_FLT_FN (BUILT_IN_CASINH):
    CASE_FLT_FN (BUILT_IN_CATAN):
    CASE_FLT_FN (BUILT_IN_CATANH):
    CASE_FLT_FN (BUILT_IN_CCOS):
    CASE_FLT_FN (BUILT_IN_CCOSH):
    CASE_FLT_FN (BUILT_IN_CEXP):
    CASE_FLT_FN (BUILT_IN_CIMAG):
    CASE_FLT_FN (BUILT_IN_CLOG):
    CASE_FLT_FN (BUILT_IN_CONJ):
    CASE_FLT_FN (BUILT_IN_CPOW):
    CASE_FLT_FN (BUILT_IN_CPROJ):
    CASE_FLT_FN (BUILT_IN_CREAL):
    CASE_FLT_FN (BUILT_IN_CSIN):
    CASE_FLT_FN (BUILT_IN_CSINH):
    CASE_FLT_FN (BUILT_IN_CSQRT):
    CASE_FLT_FN (BUILT_IN_CTAN):
    CASE_FLT_FN (BUILT_IN_CTANH):
      return "<complex.h>";
    case BUILT_IN_MEMCHR:
    case BUILT_IN_MEMCMP:
    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMMOVE:
    case BUILT_IN_MEMSET:
    case BUILT_IN_STRCAT:
    case BUILT_IN_STRCHR:
    case BUILT_IN_STRCMP:
    case BUILT_IN_STRCPY:
    case BUILT_IN_STRCSPN:
    case BUILT_IN_STRLEN:
    case BUILT_IN_STRNCAT:
    case BUILT_IN_STRNCMP:
    case BUILT_IN_STRNCPY:
    case BUILT_IN_STRPBRK:
    case BUILT_IN_STRRCHR:
    case BUILT_IN_STRSPN:
    case BUILT_IN_STRSTR:
      return "<string.h>";
    case BUILT_IN_FPRINTF:
    case BUILT_IN_PUTC:
    case BUILT_IN_FPUTC:
    case BUILT_IN_FPUTS:
    case BUILT_IN_FSCANF:
    case BUILT_IN_FWRITE:
    case BUILT_IN_PRINTF:
    case BUILT_IN_PUTCHAR:
    case BUILT_IN_PUTS:
    case BUILT_IN_SCANF:
    case BUILT_IN_SNPRINTF:
    case BUILT_IN_SPRINTF:
    case BUILT_IN_SSCANF:
    case BUILT_IN_VFPRINTF:
    case BUILT_IN_VFSCANF:
    case BUILT_IN_VPRINTF:
    case BUILT_IN_VSCANF:
    case BUILT_IN_VSNPRINTF:
    case BUILT_IN_VSPRINTF:
    case BUILT_IN_VSSCANF:
      return "<stdio.h>";
    case BUILT_IN_ISALNUM:
    case BUILT_IN_ISALPHA:
    case BUILT_IN_ISBLANK:
    case BUILT_IN_ISCNTRL:
    case BUILT_IN_ISDIGIT:
    case BUILT_IN_ISGRAPH:
    case BUILT_IN_ISLOWER:
    case BUILT_IN_ISPRINT:
    case BUILT_IN_ISPUNCT:
    case BUILT_IN_ISSPACE:
    case BUILT_IN_ISUPPER:
    case BUILT_IN_ISXDIGIT:
    case BUILT_IN_TOLOWER:
    case BUILT_IN_TOUPPER:
      return "<ctype.h>";
    case BUILT_IN_ISWALNUM:
    case BUILT_IN_ISWALPHA:
    case BUILT_IN_ISWBLANK:
    case BUILT_IN_ISWCNTRL:
    case BUILT_IN_ISWDIGIT:
    case BUILT_IN_ISWGRAPH:
    case BUILT_IN_ISWLOWER:
    case BUILT_IN_ISWPRINT:
    case BUILT_IN_ISWPUNCT:
    case BUILT_IN_ISWSPACE:
    case BUILT_IN_ISWUPPER:
    case BUILT_IN_ISWXDIGIT:
    case BUILT_IN_TOWLOWER:
    case BUILT_IN_TOWUPPER:
      return "<wctype.h>";
    case BUILT_IN_ABORT:
    case BUILT_IN_ABS:
    case BUILT_IN_CALLOC:
    case BUILT_IN_EXIT:
    case BUILT_IN_FREE:
    case BUILT_IN_LABS:
    case BUILT_IN_LLABS:
    case BUILT_IN_MALLOC:
    case BUILT_IN_REALLOC:
    case BUILT_IN__EXIT2:
    case BUILT_IN_ALIGNED_ALLOC:
      return "<stdlib.h>";
    case BUILT_IN_IMAXABS:
      return "<inttypes.h>";
    case BUILT_IN_STRFTIME:
      return "<time.h>";
    default:
      return NULL;
    }
}

/* Generate an implicit declaration for identifier FUNCTIONID at LOC as a
   function of type int ().  */

tree
implicitly_declare (location_t loc, tree functionid)
{
  struct c_binding *b;
  tree decl = NULL_TREE;
  tree asmspec_tree;

  for (b = I_SYMBOL_BINDING (functionid); b; b = b->shadowed)
    {
      if (B_IN_SCOPE (b, external_scope))
	{
	  decl = b->decl;
	  break;
	}
    }

  if (decl)
    {
      if (TREE_CODE (decl) != FUNCTION_DECL)
	return decl;

      /* FIXME: Objective-C has weird not-really-builtin functions
	 which are supposed to be visible automatically.  They wind up
	 in the external scope because they're pushed before the file
	 scope gets created.  Catch this here and rebind them into the
	 file scope.  */
      if (!DECL_BUILT_IN (decl) && DECL_IS_BUILTIN (decl))
	{
	  bind (functionid, decl, file_scope,
		/*invisible=*/false, /*nested=*/true,
		DECL_SOURCE_LOCATION (decl));
	  return decl;
	}
      else
	{
	  tree newtype = default_function_type;
	  if (b->u.type)
	    TREE_TYPE (decl) = b->u.type;
	  /* Implicit declaration of a function already declared
	     (somehow) in a different scope, or as a built-in.
	     If this is the first time this has happened, warn;
	     then recycle the old declaration but with the new type.  */
	  if (!C_DECL_IMPLICIT (decl))
	    {
	      implicit_decl_warning (loc, functionid, decl);
	      C_DECL_IMPLICIT (decl) = 1;
	    }
	  if (DECL_BUILT_IN (decl))
	    {
	      newtype = build_type_attribute_variant (newtype,
						      TYPE_ATTRIBUTES
						      (TREE_TYPE (decl)));
	      if (!comptypes (newtype, TREE_TYPE (decl)))
		{
		  bool warned = warning_at (loc, 0, "incompatible implicit "
					    "declaration of built-in "
					    "function %qD", decl);
		  /* See if we can hint which header to include.  */
		  const char *header
		    = header_for_builtin_fn (DECL_FUNCTION_CODE (decl));
		  if (header != NULL && warned)
		    {
		      rich_location richloc (line_table, loc);
		      maybe_add_include_fixit (&richloc, header);
		      inform_at_rich_loc
			(&richloc,
			 "include %qs or provide a declaration of %qD",
			 header, decl);
		    }
		  newtype = TREE_TYPE (decl);
		}
	    }
	  else
	    {
	      if (!comptypes (newtype, TREE_TYPE (decl)))
		{
		  error_at (loc, "incompatible implicit declaration of "
			    "function %qD", decl);
		  locate_old_decl (decl);
		}
	    }
	  b->u.type = TREE_TYPE (decl);
	  TREE_TYPE (decl) = newtype;
	  bind (functionid, decl, current_scope,
		/*invisible=*/false, /*nested=*/true,
		DECL_SOURCE_LOCATION (decl));
	  return decl;
	}
    }

  /* Not seen before.  */
  decl = build_decl (loc, FUNCTION_DECL, functionid, default_function_type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  C_DECL_IMPLICIT (decl) = 1;
  implicit_decl_warning (loc, functionid, 0);
  asmspec_tree = maybe_apply_renaming_pragma (decl, /*asmname=*/NULL);
  if (asmspec_tree)
    set_user_assembler_name (decl, TREE_STRING_POINTER (asmspec_tree));

  /* C89 says implicit declarations are in the innermost block.
     So we record the decl in the standard fashion.  */
  decl = pushdecl (decl);

  /* No need to call objc_check_decl here - it's a function type.  */
  rest_of_decl_compilation (decl, 0, 0);

  /* Write a record describing this implicit function declaration
     to the prototypes file (if requested).  */
  gen_aux_info_record (decl, 0, 1, 0);

  /* Possibly apply some default attributes to this implicit declaration.  */
  decl_attributes (&decl, NULL_TREE, 0);

  return decl;
}

/* Issue an error message for a reference to an undeclared variable
   ID, including a reference to a builtin outside of function-call
   context.  Establish a binding of the identifier to error_mark_node
   in an appropriate scope, which will suppress further errors for the
   same identifier.  The error message should be given location LOC.  */
void
undeclared_variable (location_t loc, tree id)
{
  static bool already = false;
  struct c_scope *scope;

  if (current_function_decl == NULL_TREE)
    {
      const char *guessed_id = lookup_name_fuzzy (id, FUZZY_LOOKUP_NAME);
      if (guessed_id)
	{
	  gcc_rich_location richloc (loc);
	  richloc.add_fixit_replace (guessed_id);
	  error_at_rich_loc (&richloc,
			     "%qE undeclared here (not in a function);"
			     " did you mean %qs?",
			     id, guessed_id);
	}
      else
	error_at (loc, "%qE undeclared here (not in a function)", id);
      scope = current_scope;
    }
  else
    {
      if (!objc_diagnose_private_ivar (id))
	{
	  const char *guessed_id = lookup_name_fuzzy (id, FUZZY_LOOKUP_NAME);
	  if (guessed_id)
	    {
	      gcc_rich_location richloc (loc);
	      richloc.add_fixit_replace (guessed_id);
	      error_at_rich_loc
		(&richloc,
		 "%qE undeclared (first use in this function);"
		 " did you mean %qs?",
		 id, guessed_id);
	    }
	  else
	    error_at (loc, "%qE undeclared (first use in this function)", id);
	}
      if (!already)
	{
          inform (loc, "each undeclared identifier is reported only"
                  " once for each function it appears in");
	  already = true;
	}

      /* If we are parsing old-style parameter decls, current_function_decl
	 will be nonnull but current_function_scope will be null.  */
      scope = current_function_scope ? current_function_scope : current_scope;
    }
  bind (id, error_mark_node, scope, /*invisible=*/false, /*nested=*/false,
	UNKNOWN_LOCATION);
}

/* Subroutine of lookup_label, declare_label, define_label: construct a
   LABEL_DECL with all the proper frills.  Also create a struct
   c_label_vars initialized for the current scope.  */

static tree
make_label (location_t location, tree name, bool defining,
	    struct c_label_vars **p_label_vars)
{
  tree label = build_decl (location, LABEL_DECL, name, void_type_node);
  DECL_CONTEXT (label) = current_function_decl;
  SET_DECL_MODE (label, VOIDmode);

  c_label_vars *label_vars = ggc_alloc<c_label_vars> ();
  label_vars->shadowed = NULL;
  set_spot_bindings (&label_vars->label_bindings, defining);
  label_vars->decls_in_scope = make_tree_vector ();
  label_vars->gotos = NULL;
  *p_label_vars = label_vars;

  return label;
}

/* Get the LABEL_DECL corresponding to identifier NAME as a label.
   Create one if none exists so far for the current function.
   This is called when a label is used in a goto expression or
   has its address taken.  */

tree
lookup_label (tree name)
{
  tree label;
  struct c_label_vars *label_vars;

  if (current_function_scope == 0)
    {
      error ("label %qE referenced outside of any function", name);
      return NULL_TREE;
    }

  /* Use a label already defined or ref'd with this name, but not if
     it is inherited from a containing function and wasn't declared
     using __label__.  */
  label = I_LABEL_DECL (name);
  if (label && (DECL_CONTEXT (label) == current_function_decl
		|| C_DECLARED_LABEL_FLAG (label)))
    {
      /* If the label has only been declared, update its apparent
	 location to point here, for better diagnostics if it
	 turns out not to have been defined.  */
      if (DECL_INITIAL (label) == NULL_TREE)
	DECL_SOURCE_LOCATION (label) = input_location;
      return label;
    }

  /* No label binding for that identifier; make one.  */
  label = make_label (input_location, name, false, &label_vars);

  /* Ordinary labels go in the current function scope.  */
  bind_label (name, label, current_function_scope, label_vars);

  return label;
}

/* Issue a warning about DECL for a goto statement at GOTO_LOC going
   to LABEL.  */

static void
warn_about_goto (location_t goto_loc, tree label, tree decl)
{
  if (variably_modified_type_p (TREE_TYPE (decl), NULL_TREE))
    error_at (goto_loc,
	      "jump into scope of identifier with variably modified type");
  else
    warning_at (goto_loc, OPT_Wjump_misses_init,
		"jump skips variable initialization");
  inform (DECL_SOURCE_LOCATION (label), "label %qD defined here", label);
  inform (DECL_SOURCE_LOCATION (decl), "%qD declared here", decl);
}

/* Look up a label because of a goto statement.  This is like
   lookup_label, but also issues any appropriate warnings.  */

tree
lookup_label_for_goto (location_t loc, tree name)
{
  tree label;
  struct c_label_vars *label_vars;
  unsigned int ix;
  tree decl;

  label = lookup_label (name);
  if (label == NULL_TREE)
    return NULL_TREE;

  /* If we are jumping to a different function, we can't issue any
     useful warnings.  */
  if (DECL_CONTEXT (label) != current_function_decl)
    {
      gcc_assert (C_DECLARED_LABEL_FLAG (label));
      return label;
    }

  label_vars = I_LABEL_BINDING (name)->u.label;

  /* If the label has not yet been defined, then push this goto on a
     list for possible later warnings.  */
  if (label_vars->label_bindings.scope == NULL)
    {
      c_goto_bindings *g = ggc_alloc<c_goto_bindings> ();

      g->loc = loc;
      set_spot_bindings (&g->goto_bindings, true);
      vec_safe_push (label_vars->gotos, g);
      return label;
    }

  /* If there are any decls in label_vars->decls_in_scope, then this
     goto has missed the declaration of the decl.  This happens for a
     case like
       int i = 1;
      lab:
       ...
       goto lab;
     Issue a warning or error.  */
  FOR_EACH_VEC_SAFE_ELT (label_vars->decls_in_scope, ix, decl)
    warn_about_goto (loc, label, decl);

  if (label_vars->label_bindings.left_stmt_expr)
    {
      error_at (loc, "jump into statement expression");
      inform (DECL_SOURCE_LOCATION (label), "label %qD defined here", label);
    }

  return label;
}

/* Make a label named NAME in the current function, shadowing silently
   any that may be inherited from containing functions or containing
   scopes.  This is called for __label__ declarations.  */

tree
declare_label (tree name)
{
  struct c_binding *b = I_LABEL_BINDING (name);
  tree label;
  struct c_label_vars *label_vars;

  /* Check to make sure that the label hasn't already been declared
     at this scope */
  if (b && B_IN_CURRENT_SCOPE (b))
    {
      error ("duplicate label declaration %qE", name);
      locate_old_decl (b->decl);

      /* Just use the previous declaration.  */
      return b->decl;
    }

  label = make_label (input_location, name, false, &label_vars);
  C_DECLARED_LABEL_FLAG (label) = 1;

  /* Declared labels go in the current scope.  */
  bind_label (name, label, current_scope, label_vars);

  return label;
}

/* When we define a label, issue any appropriate warnings if there are
   any gotos earlier in the function which jump to this label.  */

static void
check_earlier_gotos (tree label, struct c_label_vars* label_vars)
{
  unsigned int ix;
  struct c_goto_bindings *g;

  FOR_EACH_VEC_SAFE_ELT (label_vars->gotos, ix, g)
    {
      struct c_binding *b;
      struct c_scope *scope;

      /* We have a goto to this label.  The goto is going forward.  In
	 g->scope, the goto is going to skip any binding which was
	 defined after g->bindings_in_scope.  */
      if (g->goto_bindings.scope->has_jump_unsafe_decl)
	{
	  for (b = g->goto_bindings.scope->bindings;
	       b != g->goto_bindings.bindings_in_scope;
	       b = b->prev)
	    {
	      if (decl_jump_unsafe (b->decl))
		warn_about_goto (g->loc, label, b->decl);
	    }
	}

      /* We also need to warn about decls defined in any scopes
	 between the scope of the label and the scope of the goto.  */
      for (scope = label_vars->label_bindings.scope;
	   scope != g->goto_bindings.scope;
	   scope = scope->outer)
	{
	  gcc_assert (scope != NULL);
	  if (scope->has_jump_unsafe_decl)
	    {
	      if (scope == label_vars->label_bindings.scope)
		b = label_vars->label_bindings.bindings_in_scope;
	      else
		b = scope->bindings;
	      for (; b != NULL; b = b->prev)
		{
		  if (decl_jump_unsafe (b->decl))
		    warn_about_goto (g->loc, label, b->decl);
		}
	    }
	}

      if (g->goto_bindings.stmt_exprs > 0)
	{
	  error_at (g->loc, "jump into statement expression");
	  inform (DECL_SOURCE_LOCATION (label), "label %qD defined here",
		  label);
	}
    }

  /* Now that the label is defined, we will issue warnings about
     subsequent gotos to this label when we see them.  */
  vec_safe_truncate (label_vars->gotos, 0);
  label_vars->gotos = NULL;
}

/* Define a label, specifying the location in the source file.
   Return the LABEL_DECL node for the label, if the definition is valid.
   Otherwise return NULL_TREE.  */

tree
define_label (location_t location, tree name)
{
  /* Find any preexisting label with this name.  It is an error
     if that label has already been defined in this function, or
     if there is a containing function with a declared label with
     the same name.  */
  tree label = I_LABEL_DECL (name);

  if (label
      && ((DECL_CONTEXT (label) == current_function_decl
	   && DECL_INITIAL (label) != NULL_TREE)
	  || (DECL_CONTEXT (label) != current_function_decl
	      && C_DECLARED_LABEL_FLAG (label))))
    {
      error_at (location, "duplicate label %qD", label);
      locate_old_decl (label);
      return NULL_TREE;
    }
  else if (label && DECL_CONTEXT (label) == current_function_decl)
    {
      struct c_label_vars *label_vars = I_LABEL_BINDING (name)->u.label;

      /* The label has been used or declared already in this function,
	 but not defined.  Update its location to point to this
	 definition.  */
      DECL_SOURCE_LOCATION (label) = location;
      set_spot_bindings (&label_vars->label_bindings, true);

      /* Issue warnings as required about any goto statements from
	 earlier in the function.  */
      check_earlier_gotos (label, label_vars);
    }
  else
    {
      struct c_label_vars *label_vars;

      /* No label binding for that identifier; make one.  */
      label = make_label (location, name, true, &label_vars);

      /* Ordinary labels go in the current function scope.  */
      bind_label (name, label, current_function_scope, label_vars);
    }

  if (!in_system_header_at (input_location) && lookup_name (name))
    warning_at (location, OPT_Wtraditional,
		"traditional C lacks a separate namespace "
		"for labels, identifier %qE conflicts", name);

  /* Mark label as having been defined.  */
  DECL_INITIAL (label) = error_mark_node;
  return label;
}

/* Get the bindings for a new switch statement.  This is used to issue
   warnings as appropriate for jumps from the switch to case or
   default labels.  */

struct c_spot_bindings *
c_get_switch_bindings (void)
{
  struct c_spot_bindings *switch_bindings;

  switch_bindings = XNEW (struct c_spot_bindings);
  set_spot_bindings (switch_bindings, true);
  return switch_bindings;
}

void
c_release_switch_bindings (struct c_spot_bindings *bindings)
{
  gcc_assert (bindings->stmt_exprs == 0 && !bindings->left_stmt_expr);
  XDELETE (bindings);
}

/* This is called at the point of a case or default label to issue
   warnings about decls as needed.  It returns true if it found an
   error, not just a warning.  */

bool
c_check_switch_jump_warnings (struct c_spot_bindings *switch_bindings,
			      location_t switch_loc, location_t case_loc)
{
  bool saw_error;
  struct c_scope *scope;

  saw_error = false;
  for (scope = current_scope;
       scope != switch_bindings->scope;
       scope = scope->outer)
    {
      struct c_binding *b;

      gcc_assert (scope != NULL);

      if (!scope->has_jump_unsafe_decl)
	continue;

      for (b = scope->bindings; b != NULL; b = b->prev)
	{
	  if (decl_jump_unsafe (b->decl))
	    {
	      if (variably_modified_type_p (TREE_TYPE (b->decl), NULL_TREE))
		{
		  saw_error = true;
		  error_at (case_loc,
			    ("switch jumps into scope of identifier with "
			     "variably modified type"));
		}
	      else
		warning_at (case_loc, OPT_Wjump_misses_init,
			    "switch jumps over variable initialization");
	      inform (switch_loc, "switch starts here");
	      inform (DECL_SOURCE_LOCATION (b->decl), "%qD declared here",
		      b->decl);
	    }
	}
    }

  if (switch_bindings->stmt_exprs > 0)
    {
      saw_error = true;
      error_at (case_loc, "switch jumps into statement expression");
      inform (switch_loc, "switch starts here");
    }

  return saw_error;
}

/* Given NAME, an IDENTIFIER_NODE,
   return the structure (or union or enum) definition for that name.
   If THISLEVEL_ONLY is nonzero, searches only the current_scope.
   CODE says which kind of type the caller wants;
   it is RECORD_TYPE or UNION_TYPE or ENUMERAL_TYPE.
   If PLOC is not NULL and this returns non-null, it sets *PLOC to the
   location where the tag was defined.
   If the wrong kind of type is found, an error is reported.  */

static tree
lookup_tag (enum tree_code code, tree name, bool thislevel_only,
	    location_t *ploc)
{
  struct c_binding *b = I_TAG_BINDING (name);
  bool thislevel = false;

  if (!b || !b->decl)
    return NULL_TREE;

  /* We only care about whether it's in this level if
     thislevel_only was set or it might be a type clash.  */
  if (thislevel_only || TREE_CODE (b->decl) != code)
    {
      /* For our purposes, a tag in the external scope is the same as
	 a tag in the file scope.  (Primarily relevant to Objective-C
	 and its builtin structure tags, which get pushed before the
	 file scope is created.)  */
      if (B_IN_CURRENT_SCOPE (b)
	  || (current_scope == file_scope && B_IN_EXTERNAL_SCOPE (b)))
	thislevel = true;
    }

  if (thislevel_only && !thislevel)
    return NULL_TREE;

  if (TREE_CODE (b->decl) != code)
    {
      /* Definition isn't the kind we were looking for.  */
      pending_invalid_xref = name;
      pending_invalid_xref_location = input_location;

      /* If in the same binding level as a declaration as a tag
	 of a different type, this must not be allowed to
	 shadow that tag, so give the error immediately.
	 (For example, "struct foo; union foo;" is invalid.)  */
      if (thislevel)
	pending_xref_error ();
    }

  if (ploc != NULL)
    *ploc = b->locus;

  return b->decl;
}

/* Return true if a definition exists for NAME with code CODE.  */

bool
tag_exists_p (enum tree_code code, tree name)
{
  struct c_binding *b = I_TAG_BINDING (name);

  if (b == NULL || b->decl == NULL_TREE)
    return false;
  return TREE_CODE (b->decl) == code;
}

/* Print an error message now
   for a recent invalid struct, union or enum cross reference.
   We don't print them immediately because they are not invalid
   when used in the `struct foo;' construct for shadowing.  */

void
pending_xref_error (void)
{
  if (pending_invalid_xref != NULL_TREE)
    error_at (pending_invalid_xref_location, "%qE defined as wrong kind of tag",
	      pending_invalid_xref);
  pending_invalid_xref = NULL_TREE;
}


/* Look up NAME in the current scope and its superiors
   in the namespace of variables, functions and typedefs.
   Return a ..._DECL node of some kind representing its definition,
   or return NULL_TREE if it is undefined.  */

tree
lookup_name (tree name)
{
  struct c_binding *b = I_SYMBOL_BINDING (name);
  if (b && !b->invisible)
    {
      maybe_record_typedef_use (b->decl);
      return b->decl;
    }
  return NULL_TREE;
}

/* Similar to `lookup_name' but look only at the indicated scope.  */

static tree
lookup_name_in_scope (tree name, struct c_scope *scope)
{
  struct c_binding *b;

  for (b = I_SYMBOL_BINDING (name); b; b = b->shadowed)
    if (B_IN_SCOPE (b, scope))
      return b->decl;
  return NULL_TREE;
}

/* Look for the closest match for NAME within the currently valid
   scopes.

   This finds the identifier with the lowest Levenshtein distance to
   NAME.  If there are multiple candidates with equal minimal distance,
   the first one found is returned.  Scopes are searched from innermost
   outwards, and within a scope in reverse order of declaration, thus
   benefiting candidates "near" to the current scope.

   The function also looks for similar macro names to NAME, since a
   misspelled macro name will not be expanded, and hence looks like an
   identifier to the C frontend.

   It also looks for start_typename keywords, to detect "singed" vs "signed"
   typos.  */

const char *
lookup_name_fuzzy (tree name, enum lookup_name_fuzzy_kind kind)
{
  gcc_assert (TREE_CODE (name) == IDENTIFIER_NODE);

  best_match<tree, tree> bm (name);

  /* Look within currently valid scopes.  */
  for (c_scope *scope = current_scope; scope; scope = scope->outer)
    for (c_binding *binding = scope->bindings; binding; binding = binding->prev)
      {
	if (!binding->id || binding->invisible)
	  continue;
	/* Don't use bindings from implicitly declared functions,
	   as they were likely misspellings themselves.  */
	if (TREE_CODE (binding->decl) == FUNCTION_DECL)
	  if (C_DECL_IMPLICIT (binding->decl))
	    continue;
	switch (kind)
	  {
	  case FUZZY_LOOKUP_TYPENAME:
	    if (TREE_CODE (binding->decl) != TYPE_DECL)
	      continue;
	    break;

	  case FUZZY_LOOKUP_FUNCTION_NAME:
	    if (TREE_CODE (binding->decl) != FUNCTION_DECL)
	      {
		/* Allow function pointers.  */
		if ((VAR_P (binding->decl)
		     || TREE_CODE (binding->decl) == PARM_DECL)
		    && TREE_CODE (TREE_TYPE (binding->decl)) == POINTER_TYPE
		    && (TREE_CODE (TREE_TYPE (TREE_TYPE (binding->decl)))
			== FUNCTION_TYPE))
		  break;
		continue;
	      }
	    break;

	  default:
	    break;
	  }
	bm.consider (binding->id);
      }

  /* Consider macros: if the user misspelled a macro name e.g. "SOME_MACRO"
     as:
       x = SOME_OTHER_MACRO (y);
     then "SOME_OTHER_MACRO" will survive to the frontend and show up
     as a misspelled identifier.

     Use the best distance so far so that a candidate is only set if
     a macro is better than anything so far.  This allows early rejection
     (without calculating the edit distance) of macro names that must have
     distance >= bm.get_best_distance (), and means that we only get a
     non-NULL result for best_macro_match if it's better than any of
     the identifiers already checked, which avoids needless creation
     of identifiers for macro hashnodes.  */
  best_macro_match bmm (name, bm.get_best_distance (), parse_in);
  cpp_hashnode *best_macro = bmm.get_best_meaningful_candidate ();
  /* If a macro is the closest so far to NAME, use it, creating an
     identifier tree node for it.  */
  if (best_macro)
    {
      const char *id = (const char *)best_macro->ident.str;
      tree macro_as_identifier
	= get_identifier_with_length (id, best_macro->ident.len);
      bm.set_best_so_far (macro_as_identifier,
			  bmm.get_best_distance (),
			  bmm.get_best_candidate_length ());
    }

  /* Try the "start_typename" keywords to detect
     "singed" vs "signed" typos.  */
  if (kind == FUZZY_LOOKUP_TYPENAME)
    {
      for (unsigned i = 0; i < num_c_common_reswords; i++)
	{
	  const c_common_resword *resword = &c_common_reswords[i];
	  if (!c_keyword_starts_typename (resword->rid))
	    continue;
	  tree resword_identifier = ridpointers [resword->rid];
	  if (!resword_identifier)
	    continue;
	  gcc_assert (TREE_CODE (resword_identifier) == IDENTIFIER_NODE);
	  bm.consider (resword_identifier);
	}
    }

  tree best = bm.get_best_meaningful_candidate ();
  if (best)
    return IDENTIFIER_POINTER (best);
  else
    return NULL;
}


/* Create the predefined scalar types of C,
   and some nodes representing standard constants (0, 1, (void *) 0).
   Initialize the global scope.
   Make definitions for built-in primitive functions.  */

void
c_init_decl_processing (void)
{
  location_t save_loc = input_location;

  /* Initialize reserved words for parser.  */
  c_parse_init ();

  current_function_decl = NULL_TREE;

  gcc_obstack_init (&parser_obstack);

  /* Make the externals scope.  */
  push_scope ();
  external_scope = current_scope;

  /* Declarations from c_common_nodes_and_builtins must not be associated
     with this input file, lest we get differences between using and not
     using preprocessed headers.  */
  input_location = BUILTINS_LOCATION;

  c_common_nodes_and_builtins ();

  /* In C, comparisons and TRUTH_* expressions have type int.  */
  truthvalue_type_node = integer_type_node;
  truthvalue_true_node = integer_one_node;
  truthvalue_false_node = integer_zero_node;

  /* Even in C99, which has a real boolean type.  */
  pushdecl (build_decl (UNKNOWN_LOCATION, TYPE_DECL, get_identifier ("_Bool"),
			boolean_type_node));

  input_location = save_loc;

  make_fname_decl = c_make_fname_decl;
  start_fname_decls ();
}

/* Create the VAR_DECL at LOC for __FUNCTION__ etc. ID is the name to
   give the decl, NAME is the initialization string and TYPE_DEP
   indicates whether NAME depended on the type of the function.  As we
   don't yet implement delayed emission of static data, we mark the
   decl as emitted so it is not placed in the output.  Anything using
   it must therefore pull out the STRING_CST initializer directly.
   FIXME.  */

static tree
c_make_fname_decl (location_t loc, tree id, int type_dep)
{
  const char *name = fname_as_string (type_dep);
  tree decl, type, init;
  size_t length = strlen (name);

  type = build_array_type (char_type_node,
			   build_index_type (size_int (length)));
  type = c_build_qualified_type (type, TYPE_QUAL_CONST);

  decl = build_decl (loc, VAR_DECL, id, type);

  TREE_STATIC (decl) = 1;
  TREE_READONLY (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;

  init = build_string (length + 1, name);
  free (CONST_CAST (char *, name));
  TREE_TYPE (init) = type;
  DECL_INITIAL (decl) = init;

  TREE_USED (decl) = 1;

  if (current_function_decl
      /* For invalid programs like this:

         void foo()
         const char* p = __FUNCTION__;

	 the __FUNCTION__ is believed to appear in K&R style function
	 parameter declarator.  In that case we still don't have
	 function_scope.  */
      && current_function_scope)
    {
      DECL_CONTEXT (decl) = current_function_decl;
      bind (id, decl, current_function_scope,
	    /*invisible=*/false, /*nested=*/false, UNKNOWN_LOCATION);
    }

  finish_decl (decl, loc, init, NULL_TREE, NULL_TREE);

  return decl;
}

tree
c_builtin_function (tree decl)
{
  tree type = TREE_TYPE (decl);
  tree   id = DECL_NAME (decl);

  const char *name = IDENTIFIER_POINTER (id);
  C_DECL_BUILTIN_PROTOTYPE (decl) = prototype_p (type);

  /* Should never be called on a symbol with a preexisting meaning.  */
  gcc_assert (!I_SYMBOL_BINDING (id));

  bind (id, decl, external_scope, /*invisible=*/true, /*nested=*/false,
	UNKNOWN_LOCATION);

  /* Builtins in the implementation namespace are made visible without
     needing to be explicitly declared.  See push_file_scope.  */
  if (name[0] == '_' && (name[1] == '_' || ISUPPER (name[1])))
    {
      DECL_CHAIN (decl) = visible_builtins;
      visible_builtins = decl;
    }

  return decl;
}

tree
c_builtin_function_ext_scope (tree decl)
{
  tree type = TREE_TYPE (decl);
  tree   id = DECL_NAME (decl);

  const char *name = IDENTIFIER_POINTER (id);
  C_DECL_BUILTIN_PROTOTYPE (decl) = prototype_p (type);

  if (external_scope)
    bind (id, decl, external_scope, /*invisible=*/false, /*nested=*/false,
	  UNKNOWN_LOCATION);

  /* Builtins in the implementation namespace are made visible without
     needing to be explicitly declared.  See push_file_scope.  */
  if (name[0] == '_' && (name[1] == '_' || ISUPPER (name[1])))
    {
      DECL_CHAIN (decl) = visible_builtins;
      visible_builtins = decl;
    }

  return decl;
}

/* Called when a declaration is seen that contains no names to declare.
   If its type is a reference to a structure, union or enum inherited
   from a containing scope, shadow that tag name for the current scope
   with a forward reference.
   If its type defines a new named structure or union
   or defines an enum, it is valid but we need not do anything here.
   Otherwise, it is an error.  */

void
shadow_tag (const struct c_declspecs *declspecs)
{
  shadow_tag_warned (declspecs, 0);
}

/* WARNED is 1 if we have done a pedwarn, 2 if we have done a warning,
   but no pedwarn.  */
void
shadow_tag_warned (const struct c_declspecs *declspecs, int warned)
{
  bool found_tag = false;

  if (declspecs->type && !declspecs->default_int_p && !declspecs->typedef_p)
    {
      tree value = declspecs->type;
      enum tree_code code = TREE_CODE (value);

      if (code == RECORD_TYPE || code == UNION_TYPE || code == ENUMERAL_TYPE)
	/* Used to test also that TYPE_SIZE (value) != 0.
	   That caused warning for `struct foo;' at top level in the file.  */
	{
	  tree name = TYPE_NAME (value);
	  tree t;

	  found_tag = true;

	  if (declspecs->restrict_p)
	    {
	      error ("invalid use of %<restrict%>");
	      warned = 1;
	    }

	  if (name == NULL_TREE)
	    {
	      if (warned != 1 && code != ENUMERAL_TYPE)
		/* Empty unnamed enum OK */
		{
		  pedwarn (input_location, 0,
			   "unnamed struct/union that defines no instances");
		  warned = 1;
		}
	    }
	  else if (declspecs->typespec_kind != ctsk_tagdef
                   && declspecs->typespec_kind != ctsk_tagfirstref
		   && declspecs->storage_class != csc_none)
	    {
	      if (warned != 1)
		pedwarn (input_location, 0,
			 "empty declaration with storage class specifier "
			 "does not redeclare tag");
	      warned = 1;
	      pending_xref_error ();
	    }
	  else if (declspecs->typespec_kind != ctsk_tagdef
                   && declspecs->typespec_kind != ctsk_tagfirstref
		   && (declspecs->const_p
		       || declspecs->volatile_p
		       || declspecs->atomic_p
		       || declspecs->restrict_p
		       || declspecs->address_space))
	    {
	      if (warned != 1)
		pedwarn (input_location, 0,
			 "empty declaration with type qualifier "
			  "does not redeclare tag");
	      warned = 1;
	      pending_xref_error ();
	    }
	  else if (declspecs->typespec_kind != ctsk_tagdef
                   && declspecs->typespec_kind != ctsk_tagfirstref
		   && declspecs->alignas_p)
	    {
	      if (warned != 1)
		pedwarn (input_location, 0,
			 "empty declaration with %<_Alignas%> "
			  "does not redeclare tag");
	      warned = 1;
	      pending_xref_error ();
	    }
	  else
	    {
	      pending_invalid_xref = NULL_TREE;
	      t = lookup_tag (code, name, true, NULL);

	      if (t == NULL_TREE)
		{
		  t = make_node (code);
		  pushtag (input_location, name, t);
		}
	    }
	}
      else
	{
	  if (warned != 1 && !in_system_header_at (input_location))
	    {
	      pedwarn (input_location, 0,
		       "useless type name in empty declaration");
	      warned = 1;
	    }
	}
    }
  else if (warned != 1 && !in_system_header_at (input_location)
	   && declspecs->typedef_p)
    {
      pedwarn (input_location, 0, "useless type name in empty declaration");
      warned = 1;
    }

  pending_invalid_xref = NULL_TREE;

  if (declspecs->inline_p)
    {
      error ("%<inline%> in empty declaration");
      warned = 1;
    }

  if (declspecs->noreturn_p)
    {
      error ("%<_Noreturn%> in empty declaration");
      warned = 1;
    }

  if (current_scope == file_scope && declspecs->storage_class == csc_auto)
    {
      error ("%<auto%> in file-scope empty declaration");
      warned = 1;
    }

  if (current_scope == file_scope && declspecs->storage_class == csc_register)
    {
      error ("%<register%> in file-scope empty declaration");
      warned = 1;
    }

  if (!warned && !in_system_header_at (input_location)
      && declspecs->storage_class != csc_none)
    {
      warning (0, "useless storage class specifier in empty declaration");
      warned = 2;
    }

  if (!warned && !in_system_header_at (input_location) && declspecs->thread_p)
    {
      warning (0, "useless %qs in empty declaration",
	       declspecs->thread_gnu_p ? "__thread" : "_Thread_local");
      warned = 2;
    }

  if (!warned
      && !in_system_header_at (input_location)
      && (declspecs->const_p
	  || declspecs->volatile_p
	  || declspecs->atomic_p
	  || declspecs->restrict_p
	  || declspecs->address_space))
    {
      warning (0, "useless type qualifier in empty declaration");
      warned = 2;
    }

  if (!warned && !in_system_header_at (input_location)
      && declspecs->alignas_p)
    {
      warning (0, "useless %<_Alignas%> in empty declaration");
      warned = 2;
    }

  if (warned != 1)
    {
      if (!found_tag)
	pedwarn (input_location, 0, "empty declaration");
    }
}


/* Return the qualifiers from SPECS as a bitwise OR of TYPE_QUAL_*
   bits.  SPECS represents declaration specifiers that the grammar
   only permits to contain type qualifiers and attributes.  */

int
quals_from_declspecs (const struct c_declspecs *specs)
{
  int quals = ((specs->const_p ? TYPE_QUAL_CONST : 0)
	       | (specs->volatile_p ? TYPE_QUAL_VOLATILE : 0)
	       | (specs->restrict_p ? TYPE_QUAL_RESTRICT : 0)
	       | (specs->atomic_p ? TYPE_QUAL_ATOMIC : 0)
	       | (ENCODE_QUAL_ADDR_SPACE (specs->address_space)));
  gcc_assert (!specs->type
	      && !specs->decl_attr
	      && specs->typespec_word == cts_none
	      && specs->storage_class == csc_none
	      && !specs->typedef_p
	      && !specs->explicit_signed_p
	      && !specs->deprecated_p
	      && !specs->long_p
	      && !specs->long_long_p
	      && !specs->short_p
	      && !specs->signed_p
	      && !specs->unsigned_p
	      && !specs->complex_p
	      && !specs->inline_p
	      && !specs->noreturn_p
	      && !specs->thread_p);
  return quals;
}

/* Construct an array declarator.  LOC is the location of the
   beginning of the array (usually the opening brace).  EXPR is the
   expression inside [], or NULL_TREE.  QUALS are the type qualifiers
   inside the [] (to be applied to the pointer to which a parameter
   array is converted).  STATIC_P is true if "static" is inside the
   [], false otherwise.  VLA_UNSPEC_P is true if the array is [*], a
   VLA of unspecified length which is nevertheless a complete type,
   false otherwise.  The field for the contained declarator is left to
   be filled in by set_array_declarator_inner.  */

struct c_declarator *
build_array_declarator (location_t loc,
			tree expr, struct c_declspecs *quals, bool static_p,
			bool vla_unspec_p)
{
  struct c_declarator *declarator = XOBNEW (&parser_obstack,
					    struct c_declarator);
  declarator->id_loc = loc;
  declarator->kind = cdk_array;
  declarator->declarator = 0;
  declarator->u.array.dimen = expr;
  if (quals)
    {
      declarator->u.array.attrs = quals->attrs;
      declarator->u.array.quals = quals_from_declspecs (quals);
    }
  else
    {
      declarator->u.array.attrs = NULL_TREE;
      declarator->u.array.quals = 0;
    }
  declarator->u.array.static_p = static_p;
  declarator->u.array.vla_unspec_p = vla_unspec_p;
  if (static_p || quals != NULL)
    pedwarn_c90 (loc, OPT_Wpedantic,
		 "ISO C90 does not support %<static%> or type "
		 "qualifiers in parameter array declarators");
  if (vla_unspec_p)
    pedwarn_c90 (loc, OPT_Wpedantic,
		 "ISO C90 does not support %<[*]%> array declarators");
  if (vla_unspec_p)
    {
      if (!current_scope->parm_flag)
	{
	  /* C99 6.7.5.2p4 */
	  error_at (loc, "%<[*]%> not allowed in other than "
		    "function prototype scope");
	  declarator->u.array.vla_unspec_p = false;
	  return NULL;
	}
      current_scope->had_vla_unspec = true;
    }
  return declarator;
}

/* Set the contained declarator of an array declarator.  DECL is the
   declarator, as constructed by build_array_declarator; INNER is what
   appears on the left of the [].  */

struct c_declarator *
set_array_declarator_inner (struct c_declarator *decl,
			    struct c_declarator *inner)
{
  decl->declarator = inner;
  return decl;
}

/* INIT is a constructor that forms DECL's initializer.  If the final
   element initializes a flexible array field, add the size of that
   initializer to DECL's size.  */

static void
add_flexible_array_elts_to_size (tree decl, tree init)
{
  tree elt, type;

  if (vec_safe_is_empty (CONSTRUCTOR_ELTS (init)))
    return;

  elt = CONSTRUCTOR_ELTS (init)->last ().value;
  type = TREE_TYPE (elt);
  if (TREE_CODE (type) == ARRAY_TYPE
      && TYPE_SIZE (type) == NULL_TREE
      && TYPE_DOMAIN (type) != NULL_TREE
      && TYPE_MAX_VALUE (TYPE_DOMAIN (type)) == NULL_TREE)
    {
      complete_array_type (&type, elt, false);
      DECL_SIZE (decl)
	= size_binop (PLUS_EXPR, DECL_SIZE (decl), TYPE_SIZE (type));
      DECL_SIZE_UNIT (decl)
	= size_binop (PLUS_EXPR, DECL_SIZE_UNIT (decl), TYPE_SIZE_UNIT (type));
    }
}

/* Decode a "typename", such as "int **", returning a ..._TYPE node.
   Set *EXPR, if EXPR not NULL, to any expression to be evaluated
   before the type name, and set *EXPR_CONST_OPERANDS, if
   EXPR_CONST_OPERANDS not NULL, to indicate whether the type name may
   appear in a constant expression.  */

tree
groktypename (struct c_type_name *type_name, tree *expr,
	      bool *expr_const_operands)
{
  tree type;
  tree attrs = type_name->specs->attrs;

  type_name->specs->attrs = NULL_TREE;

  type = grokdeclarator (type_name->declarator, type_name->specs, TYPENAME,
			 false, NULL, &attrs, expr, expr_const_operands,
			 DEPRECATED_NORMAL);

  /* Apply attributes.  */
  decl_attributes (&type, attrs, 0);

  return type;
}

/* Wrapper for decl_attributes that adds some implicit attributes
   to VAR_DECLs or FUNCTION_DECLs.  */

static tree
c_decl_attributes (tree *node, tree attributes, int flags)
{
  /* Add implicit "omp declare target" attribute if requested.  */
  if (current_omp_declare_target_attribute
      && ((VAR_P (*node) && is_global_var (*node))
	  || TREE_CODE (*node) == FUNCTION_DECL))
    {
      if (VAR_P (*node)
	  && !lang_hooks.types.omp_mappable_type (TREE_TYPE (*node)))
	error ("%q+D in declare target directive does not have mappable type",
	       *node);
      else
	attributes = tree_cons (get_identifier ("omp declare target"),
				NULL_TREE, attributes);
    }
  return decl_attributes (node, attributes, flags);
}


/* Decode a declarator in an ordinary declaration or data definition.
   This is called as soon as the type information and variable name
   have been parsed, before parsing the initializer if any.
   Here we create the ..._DECL node, fill in its type,
   and put it on the list of decls for the current context.
   The ..._DECL node is returned as the value.

   Exception: for arrays where the length is not specified,
   the type is left null, to be filled in by `finish_decl'.

   Function definitions do not come here; they go to start_function
   instead.  However, external and forward declarations of functions
   do go through here.  Structure field declarations are done by
   grokfield and not through here.  */

tree
start_decl (struct c_declarator *declarator, struct c_declspecs *declspecs,
	    bool initialized, tree attributes)
{
  tree decl;
  tree tem;
  tree expr = NULL_TREE;
  enum deprecated_states deprecated_state = DEPRECATED_NORMAL;

  /* An object declared as __attribute__((deprecated)) suppresses
     warnings of uses of other deprecated items.  */
  if (lookup_attribute ("deprecated", attributes))
    deprecated_state = DEPRECATED_SUPPRESS;

  decl = grokdeclarator (declarator, declspecs,
			 NORMAL, initialized, NULL, &attributes, &expr, NULL,
			 deprecated_state);
  if (!decl || decl == error_mark_node)
    return NULL_TREE;

  if (expr)
    add_stmt (fold_convert (void_type_node, expr));

  if (TREE_CODE (decl) != FUNCTION_DECL && MAIN_NAME_P (DECL_NAME (decl)))
    warning (OPT_Wmain, "%q+D is usually a function", decl);

  if (initialized)
    /* Is it valid for this decl to have an initializer at all?
       If not, set INITIALIZED to zero, which will indirectly
       tell 'finish_decl' to ignore the initializer once it is parsed.  */
    switch (TREE_CODE (decl))
      {
      case TYPE_DECL:
	error ("typedef %qD is initialized (use __typeof__ instead)", decl);
	initialized = false;
	break;

      case FUNCTION_DECL:
	error ("function %qD is initialized like a variable", decl);
	initialized = false;
	break;

      case PARM_DECL:
	/* DECL_INITIAL in a PARM_DECL is really DECL_ARG_TYPE.  */
	error ("parameter %qD is initialized", decl);
	initialized = false;
	break;

      default:
	/* Don't allow initializations for incomplete types except for
	   arrays which might be completed by the initialization.  */

	/* This can happen if the array size is an undefined macro.
	   We already gave a warning, so we don't need another one.  */
	if (TREE_TYPE (decl) == error_mark_node)
	  initialized = false;
	else if (COMPLETE_TYPE_P (TREE_TYPE (decl)))
	  {
	    /* A complete type is ok if size is fixed.  */

	    if (TREE_CODE (TYPE_SIZE (TREE_TYPE (decl))) != INTEGER_CST
		|| C_DECL_VARIABLE_SIZE (decl))
	      {
		error ("variable-sized object may not be initialized");
		initialized = false;
	      }
	  }
	else if (TREE_CODE (TREE_TYPE (decl)) != ARRAY_TYPE)
	  {
	    error ("variable %qD has initializer but incomplete type", decl);
	    initialized = false;
	  }
	else if (C_DECL_VARIABLE_SIZE (decl))
	  {
	    /* Although C99 is unclear about whether incomplete arrays
	       of VLAs themselves count as VLAs, it does not make
	       sense to permit them to be initialized given that
	       ordinary VLAs may not be initialized.  */
	    error ("variable-sized object may not be initialized");
	    initialized = false;
	  }
      }

  if (initialized)
    {
      if (current_scope == file_scope)
	TREE_STATIC (decl) = 1;

      /* Tell 'pushdecl' this is an initialized decl
	 even though we don't yet have the initializer expression.
	 Also tell 'finish_decl' it may store the real initializer.  */
      DECL_INITIAL (decl) = error_mark_node;
    }

  /* If this is a function declaration, write a record describing it to the
     prototypes file (if requested).  */

  if (TREE_CODE (decl) == FUNCTION_DECL)
    gen_aux_info_record (decl, 0, 0, prototype_p (TREE_TYPE (decl)));

  /* ANSI specifies that a tentative definition which is not merged with
     a non-tentative definition behaves exactly like a definition with an
     initializer equal to zero.  (Section 3.7.2)

     -fno-common gives strict ANSI behavior, though this tends to break
     a large body of code that grew up without this rule.

     Thread-local variables are never common, since there's no entrenched
     body of code to break, and it allows more efficient variable references
     in the presence of dynamic linking.  */

  if (VAR_P (decl)
      && !initialized
      && TREE_PUBLIC (decl)
      && !DECL_THREAD_LOCAL_P (decl)
      && !flag_no_common)
    DECL_COMMON (decl) = 1;

  /* Set attributes here so if duplicate decl, will have proper attributes.  */
  c_decl_attributes (&decl, attributes, 0);

  /* Handle gnu_inline attribute.  */
  if (declspecs->inline_p
      && !flag_gnu89_inline
      && TREE_CODE (decl) == FUNCTION_DECL
      && (lookup_attribute ("gnu_inline", DECL_ATTRIBUTES (decl))
	  || current_function_decl))
    {
      if (declspecs->storage_class == csc_auto && current_scope != file_scope)
	;
      else if (declspecs->storage_class != csc_static)
	DECL_EXTERNAL (decl) = !DECL_EXTERNAL (decl);
    }

  if (TREE_CODE (decl) == FUNCTION_DECL
      && targetm.calls.promote_prototypes (TREE_TYPE (decl)))
    {
      struct c_declarator *ce = declarator;

      if (ce->kind == cdk_pointer)
	ce = declarator->declarator;
      if (ce->kind == cdk_function)
	{
	  tree args = ce->u.arg_info->parms;
	  for (; args; args = DECL_CHAIN (args))
	    {
	      tree type = TREE_TYPE (args);
	      if (type && INTEGRAL_TYPE_P (type)
		  && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
		DECL_ARG_TYPE (args) = c_type_promotes_to (type);
	    }
	}
    }

  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_DECLARED_INLINE_P (decl)
      && DECL_UNINLINABLE (decl)
      && lookup_attribute ("noinline", DECL_ATTRIBUTES (decl)))
    warning (OPT_Wattributes, "inline function %q+D given attribute noinline",
	     decl);

  /* C99 6.7.4p3: An inline definition of a function with external
     linkage shall not contain a definition of a modifiable object
     with static storage duration...  */
  if (VAR_P (decl)
      && current_scope != file_scope
      && TREE_STATIC (decl)
      && !TREE_READONLY (decl)
      && DECL_DECLARED_INLINE_P (current_function_decl)
      && DECL_EXTERNAL (current_function_decl))
    record_inline_static (input_location, current_function_decl,
			  decl, csi_modifiable);

  if (c_dialect_objc ()
      && VAR_OR_FUNCTION_DECL_P (decl))
      objc_check_global_decl (decl);

  /* Add this decl to the current scope.
     TEM may equal DECL or it may be a previous decl of the same name.  */
  tem = pushdecl (decl);

  if (initialized && DECL_EXTERNAL (tem))
    {
      DECL_EXTERNAL (tem) = 0;
      TREE_STATIC (tem) = 1;
    }

  return tem;
}

/* Subroutine of finish_decl. TYPE is the type of an uninitialized object
   DECL or the non-array element type if DECL is an uninitialized array.
   If that type has a const member, diagnose this. */

static void
diagnose_uninitialized_cst_member (tree decl, tree type)
{
  tree field;
  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
      tree field_type;
      if (TREE_CODE (field) != FIELD_DECL)
	continue;
      field_type = strip_array_types (TREE_TYPE (field));

      if (TYPE_QUALS (field_type) & TYPE_QUAL_CONST)
      	{
	  warning_at (DECL_SOURCE_LOCATION (decl), OPT_Wc___compat,
	  	      "uninitialized const member in %qT is invalid in C++",
		      strip_array_types (TREE_TYPE (decl)));
	  inform (DECL_SOURCE_LOCATION (field), "%qD should be initialized", field);
	}

      if (RECORD_OR_UNION_TYPE_P (field_type))
	diagnose_uninitialized_cst_member (decl, field_type);
    }
}

/* Finish processing of a declaration;
   install its initial value.
   If ORIGTYPE is not NULL_TREE, it is the original type of INIT.
   If the length of an array type is not known before,
   it must be determined now, from the initial value, or it is an error.

   INIT_LOC is the location of the initial value.  */

void
finish_decl (tree decl, location_t init_loc, tree init,
	     tree origtype, tree asmspec_tree)
{
  tree type;
  bool was_incomplete = (DECL_SIZE (decl) == NULL_TREE);
  const char *asmspec = 0;

  /* If a name was specified, get the string.  */
  if (VAR_OR_FUNCTION_DECL_P (decl)
      && DECL_FILE_SCOPE_P (decl))
    asmspec_tree = maybe_apply_renaming_pragma (decl, asmspec_tree);
  if (asmspec_tree)
    asmspec = TREE_STRING_POINTER (asmspec_tree);

  if (VAR_P (decl)
      && TREE_STATIC (decl)
      && global_bindings_p ())
    /* So decl is a global variable. Record the types it uses
       so that we can decide later to emit debug info for them.  */
    record_types_used_by_current_var_decl (decl);

  /* If `start_decl' didn't like having an initialization, ignore it now.  */
  if (init != NULL_TREE && DECL_INITIAL (decl) == NULL_TREE)
    init = NULL_TREE;

  /* Don't crash if parm is initialized.  */
  if (TREE_CODE (decl) == PARM_DECL)
    init = NULL_TREE;

  if (init)
    store_init_value (init_loc, decl, init, origtype);

  if (c_dialect_objc () && (VAR_OR_FUNCTION_DECL_P (decl)
			    || TREE_CODE (decl) == FIELD_DECL))
    objc_check_decl (decl);

  type = TREE_TYPE (decl);

  /* Deduce size of array from initialization, if not already known.  */
  if (TREE_CODE (type) == ARRAY_TYPE
      && TYPE_DOMAIN (type) == NULL_TREE
      && TREE_CODE (decl) != TYPE_DECL)
    {
      bool do_default
	= (TREE_STATIC (decl)
	   /* Even if pedantic, an external linkage array
	      may have incomplete type at first.  */
	   ? pedantic && !TREE_PUBLIC (decl)
	   : !DECL_EXTERNAL (decl));
      int failure
	= complete_array_type (&TREE_TYPE (decl), DECL_INITIAL (decl),
			       do_default);

      /* Get the completed type made by complete_array_type.  */
      type = TREE_TYPE (decl);

      switch (failure)
	{
	case 1:
	  error ("initializer fails to determine size of %q+D", decl);
	  break;

	case 2:
	  if (do_default)
	    error ("array size missing in %q+D", decl);
	  /* If a `static' var's size isn't known,
	     make it extern as well as static, so it does not get
	     allocated.
	     If it is not `static', then do not mark extern;
	     finish_incomplete_decl will give it a default size
	     and it will get allocated.  */
	  else if (!pedantic && TREE_STATIC (decl) && !TREE_PUBLIC (decl))
	    DECL_EXTERNAL (decl) = 1;
	  break;

	case 3:
	  error ("zero or negative size array %q+D", decl);
	  break;

	case 0:
	  /* For global variables, update the copy of the type that
	     exists in the binding.  */
	  if (TREE_PUBLIC (decl))
	    {
	      struct c_binding *b_ext = I_SYMBOL_BINDING (DECL_NAME (decl));
	      while (b_ext && !B_IN_EXTERNAL_SCOPE (b_ext))
		b_ext = b_ext->shadowed;
	      if (b_ext && TREE_CODE (decl) == TREE_CODE (b_ext->decl))
		{
		  if (b_ext->u.type && comptypes (b_ext->u.type, type))
		    b_ext->u.type = composite_type (b_ext->u.type, type);
		  else
		    b_ext->u.type = type;
		}
	    }
	  break;

	default:
	  gcc_unreachable ();
	}

      if (DECL_INITIAL (decl))
	TREE_TYPE (DECL_INITIAL (decl)) = type;

      relayout_decl (decl);
    }

  if (VAR_P (decl))
    {
      if (init && TREE_CODE (init) == CONSTRUCTOR)
	add_flexible_array_elts_to_size (decl, init);

      if (DECL_SIZE (decl) == NULL_TREE && TREE_TYPE (decl) != error_mark_node
	  && COMPLETE_TYPE_P (TREE_TYPE (decl)))
	layout_decl (decl, 0);

      if (DECL_SIZE (decl) == NULL_TREE
	  /* Don't give an error if we already gave one earlier.  */
	  && TREE_TYPE (decl) != error_mark_node
	  && (TREE_STATIC (decl)
	      /* A static variable with an incomplete type
		 is an error if it is initialized.
		 Also if it is not file scope.
		 Otherwise, let it through, but if it is not `extern'
		 then it may cause an error message later.  */
	      ? (DECL_INITIAL (decl) != NULL_TREE
		 || !DECL_FILE_SCOPE_P (decl))
	      /* An automatic variable with an incomplete type
		 is an error.  */
	      : !DECL_EXTERNAL (decl)))
	 {
	   error ("storage size of %q+D isn%'t known", decl);
	   TREE_TYPE (decl) = error_mark_node;
	 }

      if ((RECORD_OR_UNION_TYPE_P (TREE_TYPE (decl))
	  || TREE_CODE (TREE_TYPE (decl)) == ENUMERAL_TYPE)
	  && DECL_SIZE (decl) == NULL_TREE
	  && TREE_STATIC (decl))
	incomplete_record_decls.safe_push (decl);

      if (is_global_var (decl) && DECL_SIZE (decl) != NULL_TREE)
	{
	  if (TREE_CODE (DECL_SIZE (decl)) == INTEGER_CST)
	    constant_expression_warning (DECL_SIZE (decl));
	  else
	    {
	      error ("storage size of %q+D isn%'t constant", decl);
	      TREE_TYPE (decl) = error_mark_node;
	    }
	}

      if (TREE_USED (type))
	{
	  TREE_USED (decl) = 1;
	  DECL_READ_P (decl) = 1;
	}
    }

  /* If this is a function and an assembler name is specified, reset DECL_RTL
     so we can give it its new name.  Also, update builtin_decl if it
     was a normal built-in.  */
  if (TREE_CODE (decl) == FUNCTION_DECL && asmspec)
    {
      if (DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL)
	set_builtin_user_assembler_name (decl, asmspec);
      set_user_assembler_name (decl, asmspec);
    }

  /* If #pragma weak was used, mark the decl weak now.  */
  maybe_apply_pragma_weak (decl);

  /* Output the assembler code and/or RTL code for variables and functions,
     unless the type is an undefined structure or union.
     If not, it will get done when the type is completed.  */

  if (VAR_OR_FUNCTION_DECL_P (decl))
    {
      /* Determine the ELF visibility.  */
      if (TREE_PUBLIC (decl))
	c_determine_visibility (decl);

      /* This is a no-op in c-lang.c or something real in objc-act.c.  */
      if (c_dialect_objc ())
	objc_check_decl (decl);

      if (asmspec)
	{
	  /* If this is not a static variable, issue a warning.
	     It doesn't make any sense to give an ASMSPEC for an
	     ordinary, non-register local variable.  Historically,
	     GCC has accepted -- but ignored -- the ASMSPEC in
	     this case.  */
	  if (!DECL_FILE_SCOPE_P (decl)
	      && VAR_P (decl)
	      && !C_DECL_REGISTER (decl)
	      && !TREE_STATIC (decl))
	    warning (0, "ignoring asm-specifier for non-static local "
		     "variable %q+D", decl);
	  else
	    set_user_assembler_name (decl, asmspec);
	}

      if (DECL_FILE_SCOPE_P (decl))
	{
	  if (DECL_INITIAL (decl) == NULL_TREE
	      || DECL_INITIAL (decl) == error_mark_node)
	    /* Don't output anything
	       when a tentative file-scope definition is seen.
	       But at end of compilation, do output code for them.  */
	    DECL_DEFER_OUTPUT (decl) = 1;
	  if (asmspec && VAR_P (decl) && C_DECL_REGISTER (decl))
	    DECL_HARD_REGISTER (decl) = 1;
	  rest_of_decl_compilation (decl, true, 0);
	}
      else
	{
	  /* In conjunction with an ASMSPEC, the `register'
	     keyword indicates that we should place the variable
	     in a particular register.  */
	  if (asmspec && C_DECL_REGISTER (decl))
	    {
	      DECL_HARD_REGISTER (decl) = 1;
	      /* This cannot be done for a structure with volatile
		 fields, on which DECL_REGISTER will have been
		 reset.  */
	      if (!DECL_REGISTER (decl))
		error ("cannot put object with volatile field into register");
	    }

	  if (TREE_CODE (decl) != FUNCTION_DECL)
	    {
	      /* If we're building a variable sized type, and we might be
		 reachable other than via the top of the current binding
		 level, then create a new BIND_EXPR so that we deallocate
		 the object at the right time.  */
	      /* Note that DECL_SIZE can be null due to errors.  */
	      if (DECL_SIZE (decl)
		  && !TREE_CONSTANT (DECL_SIZE (decl))
		  && STATEMENT_LIST_HAS_LABEL (cur_stmt_list))
		{
		  tree bind;
		  bind = build3 (BIND_EXPR, void_type_node, NULL, NULL, NULL);
		  TREE_SIDE_EFFECTS (bind) = 1;
		  add_stmt (bind);
		  BIND_EXPR_BODY (bind) = push_stmt_list ();
		}
	      add_stmt (build_stmt (DECL_SOURCE_LOCATION (decl),
				    DECL_EXPR, decl));
	    }
	}


      if (!DECL_FILE_SCOPE_P (decl))
	{
	  /* Recompute the RTL of a local array now
	     if it used to be an incomplete type.  */
	  if (was_incomplete && !is_global_var (decl))
	    {
	      /* If we used it already as memory, it must stay in memory.  */
	      TREE_ADDRESSABLE (decl) = TREE_USED (decl);
	      /* If it's still incomplete now, no init will save it.  */
	      if (DECL_SIZE (decl) == NULL_TREE)
		DECL_INITIAL (decl) = NULL_TREE;
	    }
	}
    }

  if (TREE_CODE (decl) == TYPE_DECL)
    {
      if (!DECL_FILE_SCOPE_P (decl)
	  && variably_modified_type_p (TREE_TYPE (decl), NULL_TREE))
	add_stmt (build_stmt (DECL_SOURCE_LOCATION (decl), DECL_EXPR, decl));

      rest_of_decl_compilation (decl, DECL_FILE_SCOPE_P (decl), 0);
    }

  /* Install a cleanup (aka destructor) if one was given.  */
  if (VAR_P (decl) && !TREE_STATIC (decl))
    {
      tree attr = lookup_attribute ("cleanup", DECL_ATTRIBUTES (decl));
      if (attr)
	{
	  tree cleanup_id = TREE_VALUE (TREE_VALUE (attr));
	  tree cleanup_decl = lookup_name (cleanup_id);
	  tree cleanup;
	  vec<tree, va_gc> *v;

	  /* Build "cleanup(&decl)" for the destructor.  */
	  cleanup = build_unary_op (input_location, ADDR_EXPR, decl, false);
	  vec_alloc (v, 1);
	  v->quick_push (cleanup);
	  cleanup = c_build_function_call_vec (DECL_SOURCE_LOCATION (decl),
					       vNULL, cleanup_decl, v, NULL);
	  vec_free (v);

	  /* Don't warn about decl unused; the cleanup uses it.  */
	  TREE_USED (decl) = 1;
	  TREE_USED (cleanup_decl) = 1;
	  DECL_READ_P (decl) = 1;

	  push_cleanup (decl, cleanup, false);
	}
    }

  if (warn_cxx_compat
      && VAR_P (decl)
      && !DECL_EXTERNAL (decl)
      && DECL_INITIAL (decl) == NULL_TREE)
    {
      type = strip_array_types (type);
      if (TREE_READONLY (decl))
	warning_at (DECL_SOURCE_LOCATION (decl), OPT_Wc___compat,
		    "uninitialized const %qD is invalid in C++", decl);
      else if (RECORD_OR_UNION_TYPE_P (type)
	       && C_TYPE_FIELDS_READONLY (type))
	diagnose_uninitialized_cst_member (decl, type);
    }

	invoke_plugin_callbacks (PLUGIN_FINISH_DECL, decl);
}

/* Given a parsed parameter declaration, decode it into a PARM_DECL.
   EXPR is NULL or a pointer to an expression that needs to be
   evaluated for the side effects of array size expressions in the
   parameters.  */

tree
grokparm (const struct c_parm *parm, tree *expr)
{
  tree attrs = parm->attrs;
  tree decl = grokdeclarator (parm->declarator, parm->specs, PARM, false,
			      NULL, &attrs, expr, NULL, DEPRECATED_NORMAL);

  decl_attributes (&decl, attrs, 0);

  return decl;
}

/* Given a parsed parameter declaration, decode it into a PARM_DECL
   and push that on the current scope.  EXPR is a pointer to an
   expression that needs to be evaluated for the side effects of array
   size expressions in the parameters.  */

void
push_parm_decl (const struct c_parm *parm, tree *expr)
{
  tree attrs = parm->attrs;
  tree decl;

  decl = grokdeclarator (parm->declarator, parm->specs, PARM, false, NULL,
			 &attrs, expr, NULL, DEPRECATED_NORMAL);
  if (decl && DECL_P (decl))
    DECL_SOURCE_LOCATION (decl) = parm->loc;
  decl_attributes (&decl, attrs, 0);

  decl = pushdecl (decl);

  finish_decl (decl, input_location, NULL_TREE, NULL_TREE, NULL_TREE);
}

/* Mark all the parameter declarations to date as forward decls.
   Also diagnose use of this extension.  */

void
mark_forward_parm_decls (void)
{
  struct c_binding *b;

  if (pedantic && !current_scope->warned_forward_parm_decls)
    {
      pedwarn (input_location, OPT_Wpedantic,
	       "ISO C forbids forward parameter declarations");
      current_scope->warned_forward_parm_decls = true;
    }

  for (b = current_scope->bindings; b; b = b->prev)
    if (TREE_CODE (b->decl) == PARM_DECL)
      TREE_ASM_WRITTEN (b->decl) = 1;
}

/* Build a COMPOUND_LITERAL_EXPR.  TYPE is the type given in the compound
   literal, which may be an incomplete array type completed by the
   initializer; INIT is a CONSTRUCTOR at LOC that initializes the compound
   literal.  NON_CONST is true if the initializers contain something
   that cannot occur in a constant expression.  */

tree
build_compound_literal (location_t loc, tree type, tree init, bool non_const)
{
  /* We do not use start_decl here because we have a type, not a declarator;
     and do not use finish_decl because the decl should be stored inside
     the COMPOUND_LITERAL_EXPR rather than added elsewhere as a DECL_EXPR.  */
  tree decl;
  tree complit;
  tree stmt;

  if (type == error_mark_node
      || init == error_mark_node)
    return error_mark_node;

  decl = build_decl (loc, VAR_DECL, NULL_TREE, type);
  DECL_EXTERNAL (decl) = 0;
  TREE_PUBLIC (decl) = 0;
  TREE_STATIC (decl) = (current_scope == file_scope);
  DECL_CONTEXT (decl) = current_function_decl;
  TREE_USED (decl) = 1;
  DECL_READ_P (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  TREE_TYPE (decl) = type;
  c_apply_type_quals_to_decl (TYPE_QUALS (strip_array_types (type)), decl);
  store_init_value (loc, decl, init, NULL_TREE);

  if (TREE_CODE (type) == ARRAY_TYPE && !COMPLETE_TYPE_P (type))
    {
      int failure = complete_array_type (&TREE_TYPE (decl),
					 DECL_INITIAL (decl), true);
      /* If complete_array_type returns 3, it means that the
         initial value of the compound literal is empty.  Allow it.  */
      gcc_assert (failure == 0 || failure == 3);

      type = TREE_TYPE (decl);
      TREE_TYPE (DECL_INITIAL (decl)) = type;
    }

  if (type == error_mark_node || !COMPLETE_TYPE_P (type))
    {
      c_incomplete_type_error (loc, NULL_TREE, type);
      return error_mark_node;
    }

  stmt = build_stmt (DECL_SOURCE_LOCATION (decl), DECL_EXPR, decl);
  complit = build1 (COMPOUND_LITERAL_EXPR, type, stmt);
  TREE_SIDE_EFFECTS (complit) = 1;

  layout_decl (decl, 0);

  if (TREE_STATIC (decl))
    {
      /* This decl needs a name for the assembler output.  */
      set_compound_literal_name (decl);
      DECL_DEFER_OUTPUT (decl) = 1;
      DECL_COMDAT (decl) = 1;
      pushdecl (decl);
      rest_of_decl_compilation (decl, 1, 0);
    }

  if (non_const)
    {
      complit = build2 (C_MAYBE_CONST_EXPR, type, NULL, complit);
      C_MAYBE_CONST_EXPR_NON_CONST (complit) = 1;
    }

  return complit;
}

/* Check the type of a compound literal.  Here we just check that it
   is valid for C++.  */

void
check_compound_literal_type (location_t loc, struct c_type_name *type_name)
{
  if (warn_cxx_compat
      && (type_name->specs->typespec_kind == ctsk_tagdef
          || type_name->specs->typespec_kind == ctsk_tagfirstref))
    warning_at (loc, OPT_Wc___compat,
		"defining a type in a compound literal is invalid in C++");
}

/* Determine whether TYPE is a structure with a flexible array member,
   or a union containing such a structure (possibly recursively).  */

static bool
flexible_array_type_p (tree type)
{
  tree x;
  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
      x = TYPE_FIELDS (type);
      if (x == NULL_TREE)
	return false;
      while (DECL_CHAIN (x) != NULL_TREE)
	x = DECL_CHAIN (x);
      if (TREE_CODE (TREE_TYPE (x)) == ARRAY_TYPE
	  && TYPE_SIZE (TREE_TYPE (x)) == NULL_TREE
	  && TYPE_DOMAIN (TREE_TYPE (x)) != NULL_TREE
	  && TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (x))) == NULL_TREE)
	return true;
      return false;
    case UNION_TYPE:
      for (x = TYPE_FIELDS (type); x != NULL_TREE; x = DECL_CHAIN (x))
	{
	  if (flexible_array_type_p (TREE_TYPE (x)))
	    return true;
	}
      return false;
    default:
    return false;
  }
}

/* Performs sanity checks on the TYPE and WIDTH of the bit-field NAME,
   replacing with appropriate values if they are invalid.  */

static void
check_bitfield_type_and_width (location_t loc, tree *type, tree *width,
			       tree orig_name)
{
  tree type_mv;
  unsigned int max_width;
  unsigned HOST_WIDE_INT w;
  const char *name = (orig_name
		      ? identifier_to_locale (IDENTIFIER_POINTER (orig_name))
		      : _("<anonymous>"));

  /* Detect and ignore out of range field width and process valid
     field widths.  */
  if (!INTEGRAL_TYPE_P (TREE_TYPE (*width)))
    {
      error_at (loc, "bit-field %qs width not an integer constant", name);
      *width = integer_one_node;
    }
  else
    {
      if (TREE_CODE (*width) != INTEGER_CST)
	{
	  *width = c_fully_fold (*width, false, NULL);
	  if (TREE_CODE (*width) == INTEGER_CST)
	    pedwarn (loc, OPT_Wpedantic,
		     "bit-field %qs width not an integer constant expression",
		     name);
	}
      if (TREE_CODE (*width) != INTEGER_CST)
	{
	  error_at (loc, "bit-field %qs width not an integer constant", name);
	  *width = integer_one_node;
	}
      constant_expression_warning (*width);
      if (tree_int_cst_sgn (*width) < 0)
	{
	  error_at (loc, "negative width in bit-field %qs", name);
	  *width = integer_one_node;
	}
      else if (integer_zerop (*width) && orig_name)
	{
	  error_at (loc, "zero width for bit-field %qs", name);
	  *width = integer_one_node;
	}
    }

  /* Detect invalid bit-field type.  */
  if (TREE_CODE (*type) != INTEGER_TYPE
      && TREE_CODE (*type) != BOOLEAN_TYPE
      && TREE_CODE (*type) != ENUMERAL_TYPE)
    {
      error_at (loc, "bit-field %qs has invalid type", name);
      *type = unsigned_type_node;
    }

  if (TYPE_WARN_IF_NOT_ALIGN (*type))
    {
      error_at (loc, "cannot declare bit-field %qs with %<warn_if_not_aligned%> type",
		name);
      *type = unsigned_type_node;
    }

  type_mv = TYPE_MAIN_VARIANT (*type);
  if (!in_system_header_at (input_location)
      && type_mv != integer_type_node
      && type_mv != unsigned_type_node
      && type_mv != boolean_type_node)
    pedwarn_c90 (loc, OPT_Wpedantic,
		 "type of bit-field %qs is a GCC extension", name);

  max_width = TYPE_PRECISION (*type);

  if (0 < compare_tree_int (*width, max_width))
    {
      error_at (loc, "width of %qs exceeds its type", name);
      w = max_width;
      *width = build_int_cst (integer_type_node, w);
    }
  else
    w = tree_to_uhwi (*width);

  if (TREE_CODE (*type) == ENUMERAL_TYPE)
    {
      struct lang_type *lt = TYPE_LANG_SPECIFIC (*type);
      if (!lt
	  || w < tree_int_cst_min_precision (lt->enum_min, TYPE_SIGN (*type))
	  || w < tree_int_cst_min_precision (lt->enum_max, TYPE_SIGN (*type)))
	warning_at (loc, 0, "%qs is narrower than values of its type", name);
    }
}



/* Print warning about variable length array if necessary.  */

static void
warn_variable_length_array (tree name, tree size)
{
  if (TREE_CONSTANT (size))
    {
      if (name)
	pedwarn_c90 (input_location, OPT_Wvla,
		     "ISO C90 forbids array %qE whose size "
		     "can%'t be evaluated", name);
      else
	pedwarn_c90 (input_location, OPT_Wvla, "ISO C90 forbids array "
		     "whose size can%'t be evaluated");
    }
  else
    {
      if (name)
	pedwarn_c90 (input_location, OPT_Wvla,
		     "ISO C90 forbids variable length array %qE", name);
      else
	pedwarn_c90 (input_location, OPT_Wvla, "ISO C90 forbids variable "
		     "length array");
    }
}

/* Print warning about defaulting to int if necessary.  */

static void
warn_defaults_to (location_t location, int opt, const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  rich_location richloc (line_table, location);

  va_start (ap, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &ap, &richloc,
                       flag_isoc99 ? DK_PEDWARN : DK_WARNING);
  diagnostic.option_index = opt;
  diagnostic_report_diagnostic (global_dc, &diagnostic);
  va_end (ap);
}

/* Returns the smallest location != UNKNOWN_LOCATION in LOCATIONS,
   considering only those c_declspec_words found in LIST, which
   must be terminated by cdw_number_of_elements.  */

static location_t
smallest_type_quals_location (const location_t *locations,
			      const c_declspec_word *list)
{
  location_t loc = UNKNOWN_LOCATION;
  while (*list != cdw_number_of_elements)
    {
      location_t newloc = locations[*list];
      if (loc == UNKNOWN_LOCATION
	  || (newloc != UNKNOWN_LOCATION && newloc < loc))
	loc = newloc;
      list++;
    }

  return loc;
}

/* Given declspecs and a declarator,
   determine the name and type of the object declared
   and construct a ..._DECL node for it.
   (In one case we can return a ..._TYPE node instead.
    For invalid input we sometimes return NULL_TREE.)

   DECLSPECS is a c_declspecs structure for the declaration specifiers.

   DECL_CONTEXT says which syntactic context this declaration is in:
     NORMAL for most contexts.  Make a VAR_DECL or FUNCTION_DECL or TYPE_DECL.
     FUNCDEF for a function definition.  Like NORMAL but a few different
      error messages in each case.  Return value may be zero meaning
      this definition is too screwy to try to parse.
     PARM for a parameter declaration (either within a function prototype
      or before a function body).  Make a PARM_DECL, or return void_type_node.
     TYPENAME if for a typename (in a cast or sizeof).
      Don't make a DECL node; just return the ..._TYPE node.
     FIELD for a struct or union field; make a FIELD_DECL.
   INITIALIZED is true if the decl has an initializer.
   WIDTH is non-NULL for bit-fields, and is a pointer to an INTEGER_CST node
   representing the width of the bit-field.
   DECL_ATTRS points to the list of attributes that should be added to this
     decl.  Any nested attributes that belong on the decl itself will be
     added to this list.
   If EXPR is not NULL, any expressions that need to be evaluated as
     part of evaluating variably modified types will be stored in *EXPR.
   If EXPR_CONST_OPERANDS is not NULL, *EXPR_CONST_OPERANDS will be
     set to indicate whether operands in *EXPR can be used in constant
     expressions.
   DEPRECATED_STATE is a deprecated_states value indicating whether
   deprecation warnings should be suppressed.

   In the TYPENAME case, DECLARATOR is really an absolute declarator.
   It may also be so in the PARM case, for a prototype where the
   argument type is specified but not the name.

   This function is where the complicated C meanings of `static'
   and `extern' are interpreted.  */

static tree
grokdeclarator (const struct c_declarator *declarator,
		struct c_declspecs *declspecs,
		enum decl_context decl_context, bool initialized, tree *width,
		tree *decl_attrs, tree *expr, bool *expr_const_operands,
		enum deprecated_states deprecated_state)
{
  tree type = declspecs->type;
  bool threadp = declspecs->thread_p;
  enum c_storage_class storage_class = declspecs->storage_class;
  int constp;
  int restrictp;
  int volatilep;
  int atomicp;
  int type_quals = TYPE_UNQUALIFIED;
  tree name = NULL_TREE;
  bool funcdef_flag = false;
  bool funcdef_syntax = false;
  bool size_varies = false;
  tree decl_attr = declspecs->decl_attr;
  int array_ptr_quals = TYPE_UNQUALIFIED;
  tree array_ptr_attrs = NULL_TREE;
  bool array_parm_static = false;
  bool array_parm_vla_unspec_p = false;
  tree returned_attrs = NULL_TREE;
  bool bitfield = width != NULL;
  tree element_type;
  tree orig_qual_type = NULL;
  size_t orig_qual_indirect = 0;
  struct c_arg_info *arg_info = 0;
  addr_space_t as1, as2, address_space;
  location_t loc = UNKNOWN_LOCATION;
  tree expr_dummy;
  bool expr_const_operands_dummy;
  enum c_declarator_kind first_non_attr_kind;
  unsigned int alignas_align = 0;

  if (TREE_CODE (type) == ERROR_MARK)
    return error_mark_node;
  if (expr == NULL)
    {
      expr = &expr_dummy;
      expr_dummy = NULL_TREE;
    }
  if (expr_const_operands == NULL)
    expr_const_operands = &expr_const_operands_dummy;

  if (declspecs->expr)
    {
      if (*expr)
	*expr = build2 (COMPOUND_EXPR, TREE_TYPE (declspecs->expr), *expr,
			declspecs->expr);
      else
	*expr = declspecs->expr;
    }
  *expr_const_operands = declspecs->expr_const_operands;

  if (decl_context == FUNCDEF)
    funcdef_flag = true, decl_context = NORMAL;

  /* Look inside a declarator for the name being declared
     and get it as an IDENTIFIER_NODE, for an error message.  */
  {
    const struct c_declarator *decl = declarator;

    first_non_attr_kind = cdk_attrs;
    while (decl)
      switch (decl->kind)
	{
	case cdk_array:
	  loc = decl->id_loc;
	  /* FALL THRU.  */

	case cdk_function:
	case cdk_pointer:
	  funcdef_syntax = (decl->kind == cdk_function);
	  if (first_non_attr_kind == cdk_attrs)
	    first_non_attr_kind = decl->kind;
	  decl = decl->declarator;
	  break;

	case cdk_attrs:
	  decl = decl->declarator;
	  break;

	case cdk_id:
	  loc = decl->id_loc;
	  if (decl->u.id)
	    name = decl->u.id;
	  if (first_non_attr_kind == cdk_attrs)
	    first_non_attr_kind = decl->kind;
	  decl = 0;
	  break;

	default:
	  gcc_unreachable ();
	}
    if (name == NULL_TREE)
      {
	gcc_assert (decl_context == PARM
		    || decl_context == TYPENAME
		    || (decl_context == FIELD
			&& declarator->kind == cdk_id));
	gcc_assert (!initialized);
      }
  }

  /* A function definition's declarator must have the form of
     a function declarator.  */

  if (funcdef_flag && !funcdef_syntax)
    return NULL_TREE;

  /* If this looks like a function definition, make it one,
     even if it occurs where parms are expected.
     Then store_parm_decls will reject it and not use it as a parm.  */
  if (decl_context == NORMAL && !funcdef_flag && current_scope->parm_flag)
    decl_context = PARM;

  if (declspecs->deprecated_p && deprecated_state != DEPRECATED_SUPPRESS)
    warn_deprecated_use (declspecs->type, declspecs->decl_attr);

  if ((decl_context == NORMAL || decl_context == FIELD)
      && current_scope == file_scope
      && variably_modified_type_p (type, NULL_TREE))
    {
      if (name)
	error_at (loc, "variably modified %qE at file scope", name);
      else
	error_at (loc, "variably modified field at file scope");
      type = integer_type_node;
    }

  size_varies = C_TYPE_VARIABLE_SIZE (type) != 0;

  /* Diagnose defaulting to "int".  */

  if (declspecs->default_int_p && !in_system_header_at (input_location))
    {
      /* Issue a warning if this is an ISO C 99 program or if
	 -Wreturn-type and this is a function, or if -Wimplicit;
	 prefer the former warning since it is more explicit.  */
      if ((warn_implicit_int || warn_return_type || flag_isoc99)
	  && funcdef_flag)
	warn_about_return_type = 1;
      else
	{
	  if (name)
	    warn_defaults_to (loc, OPT_Wimplicit_int,
			      "type defaults to %<int%> in declaration "
			      "of %qE", name);
	  else
	    warn_defaults_to (loc, OPT_Wimplicit_int,
			      "type defaults to %<int%> in type name");
	}
    }

  /* Adjust the type if a bit-field is being declared,
     -funsigned-bitfields applied and the type is not explicitly
     "signed".  */
  if (bitfield && !flag_signed_bitfields && !declspecs->explicit_signed_p
      && TREE_CODE (type) == INTEGER_TYPE)
    type = unsigned_type_for (type);

  /* Figure out the type qualifiers for the declaration.  There are
     two ways a declaration can become qualified.  One is something
     like `const int i' where the `const' is explicit.  Another is
     something like `typedef const int CI; CI i' where the type of the
     declaration contains the `const'.  A third possibility is that
     there is a type qualifier on the element type of a typedefed
     array type, in which case we should extract that qualifier so
     that c_apply_type_quals_to_decl receives the full list of
     qualifiers to work with (C90 is not entirely clear about whether
     duplicate qualifiers should be diagnosed in this case, but it
     seems most appropriate to do so).  */
  element_type = strip_array_types (type);
  constp = declspecs->const_p + TYPE_READONLY (element_type);
  restrictp = declspecs->restrict_p + TYPE_RESTRICT (element_type);
  volatilep = declspecs->volatile_p + TYPE_VOLATILE (element_type);
  atomicp = declspecs->atomic_p + TYPE_ATOMIC (element_type);
  as1 = declspecs->address_space;
  as2 = TYPE_ADDR_SPACE (element_type);
  address_space = ADDR_SPACE_GENERIC_P (as1)? as2 : as1;

  if (constp > 1)
    pedwarn_c90 (loc, OPT_Wpedantic, "duplicate %<const%>");
  if (restrictp > 1)
    pedwarn_c90 (loc, OPT_Wpedantic, "duplicate %<restrict%>");
  if (volatilep > 1)
    pedwarn_c90 (loc, OPT_Wpedantic, "duplicate %<volatile%>");
  if (atomicp > 1)
    pedwarn_c90 (loc, OPT_Wpedantic, "duplicate %<_Atomic%>");

  if (!ADDR_SPACE_GENERIC_P (as1) && !ADDR_SPACE_GENERIC_P (as2) && as1 != as2)
    error_at (loc, "conflicting named address spaces (%s vs %s)",
	      c_addr_space_name (as1), c_addr_space_name (as2));

  if ((TREE_CODE (type) == ARRAY_TYPE
       || first_non_attr_kind == cdk_array)
      && TYPE_QUALS (element_type))
    {
      orig_qual_type = type;
      type = TYPE_MAIN_VARIANT (type);
    }
  type_quals = ((constp ? TYPE_QUAL_CONST : 0)
		| (restrictp ? TYPE_QUAL_RESTRICT : 0)
		| (volatilep ? TYPE_QUAL_VOLATILE : 0)
		| (atomicp ? TYPE_QUAL_ATOMIC : 0)
		| ENCODE_QUAL_ADDR_SPACE (address_space));
  if (type_quals != TYPE_QUALS (element_type))
    orig_qual_type = NULL_TREE;

  /* Applying the _Atomic qualifier to an array type (through the use
     of typedefs or typeof) must be detected here.  If the qualifier
     is introduced later, any appearance of applying it to an array is
     actually applying it to an element of that array.  */
  if (atomicp && TREE_CODE (type) == ARRAY_TYPE)
    error_at (loc, "%<_Atomic%>-qualified array type");

  /* Warn about storage classes that are invalid for certain
     kinds of declarations (parameters, typenames, etc.).  */

  if (funcdef_flag
      && (threadp
	  || storage_class == csc_auto
	  || storage_class == csc_register
	  || storage_class == csc_typedef))
    {
      if (storage_class == csc_auto)
	pedwarn (loc,
		 (current_scope == file_scope) ? 0 : OPT_Wpedantic,
		 "function definition declared %<auto%>");
      if (storage_class == csc_register)
	error_at (loc, "function definition declared %<register%>");
      if (storage_class == csc_typedef)
	error_at (loc, "function definition declared %<typedef%>");
      if (threadp)
	error_at (loc, "function definition declared %qs",
		  declspecs->thread_gnu_p ? "__thread" : "_Thread_local");
      threadp = false;
      if (storage_class == csc_auto
	  || storage_class == csc_register
	  || storage_class == csc_typedef)
	storage_class = csc_none;
    }
  else if (decl_context != NORMAL && (storage_class != csc_none || threadp))
    {
      if (decl_context == PARM && storage_class == csc_register)
	;
      else
	{
	  switch (decl_context)
	    {
	    case FIELD:
	      if (name)
		error_at (loc, "storage class specified for structure "
		    	  "field %qE", name);
	      else
		error_at (loc, "storage class specified for structure field");
	      break;
	    case PARM:
	      if (name)
		error_at (loc, "storage class specified for parameter %qE",
		    	  name);
	      else
		error_at (loc, "storage class specified for unnamed parameter");
	      break;
	    default:
	      error_at (loc, "storage class specified for typename");
	      break;
	    }
	  storage_class = csc_none;
	  threadp = false;
	}
    }
  else if (storage_class == csc_extern
	   && initialized
	   && !funcdef_flag)
    {
      /* 'extern' with initialization is invalid if not at file scope.  */
       if (current_scope == file_scope)
         {
           /* It is fine to have 'extern const' when compiling at C
              and C++ intersection.  */
           if (!(warn_cxx_compat && constp))
             warning_at (loc, 0, "%qE initialized and declared %<extern%>",
		 	 name);
         }
      else
	error_at (loc, "%qE has both %<extern%> and initializer", name);
    }
  else if (current_scope == file_scope)
    {
      if (storage_class == csc_auto)
	error_at (loc, "file-scope declaration of %qE specifies %<auto%>",
	    	  name);
      if (pedantic && storage_class == csc_register)
	pedwarn (input_location, OPT_Wpedantic,
		 "file-scope declaration of %qE specifies %<register%>", name);
    }
  else
    {
      if (storage_class == csc_extern && funcdef_flag)
	error_at (loc, "nested function %qE declared %<extern%>", name);
      else if (threadp && storage_class == csc_none)
	{
	  error_at (loc, "function-scope %qE implicitly auto and declared "
		    "%qs", name,
		    declspecs->thread_gnu_p ? "__thread" : "_Thread_local");
	  threadp = false;
	}
    }

  /* Now figure out the structure of the declarator proper.
     Descend through it, creating more complex types, until we reach
     the declared identifier (or NULL_TREE, in an absolute declarator).
     At each stage we maintain an unqualified version of the type
     together with any qualifiers that should be applied to it with
     c_build_qualified_type; this way, array types including
     multidimensional array types are first built up in unqualified
     form and then the qualified form is created with
     TYPE_MAIN_VARIANT pointing to the unqualified form.  */

  while (declarator && declarator->kind != cdk_id)
    {
      if (type == error_mark_node)
	{
	  declarator = declarator->declarator;
	  continue;
	}

      /* Each level of DECLARATOR is either a cdk_array (for ...[..]),
	 a cdk_pointer (for *...),
	 a cdk_function (for ...(...)),
	 a cdk_attrs (for nested attributes),
	 or a cdk_id (for the name being declared
	 or the place in an absolute declarator
	 where the name was omitted).
	 For the last case, we have just exited the loop.

	 At this point, TYPE is the type of elements of an array,
	 or for a function to return, or for a pointer to point to.
	 After this sequence of ifs, TYPE is the type of the
	 array or function or pointer, and DECLARATOR has had its
	 outermost layer removed.  */

      if (array_ptr_quals != TYPE_UNQUALIFIED
	  || array_ptr_attrs != NULL_TREE
	  || array_parm_static)
	{
	  /* Only the innermost declarator (making a parameter be of
	     array type which is converted to pointer type)
	     may have static or type qualifiers.  */
	  error_at (loc, "static or type qualifiers in non-parameter array declarator");
	  array_ptr_quals = TYPE_UNQUALIFIED;
	  array_ptr_attrs = NULL_TREE;
	  array_parm_static = false;
	}

      switch (declarator->kind)
	{
	case cdk_attrs:
	  {
	    /* A declarator with embedded attributes.  */
	    tree attrs = declarator->u.attrs;
	    const struct c_declarator *inner_decl;
	    int attr_flags = 0;
	    declarator = declarator->declarator;
	    inner_decl = declarator;
	    while (inner_decl->kind == cdk_attrs)
	      inner_decl = inner_decl->declarator;
	    if (inner_decl->kind == cdk_id)
	      attr_flags |= (int) ATTR_FLAG_DECL_NEXT;
	    else if (inner_decl->kind == cdk_function)
	      attr_flags |= (int) ATTR_FLAG_FUNCTION_NEXT;
	    else if (inner_decl->kind == cdk_array)
	      attr_flags |= (int) ATTR_FLAG_ARRAY_NEXT;
	    returned_attrs = decl_attributes (&type,
					      chainon (returned_attrs, attrs),
					      attr_flags);
	    break;
	  }
	case cdk_array:
	  {
	    tree itype = NULL_TREE;
	    tree size = declarator->u.array.dimen;
	    /* The index is a signed object `sizetype' bits wide.  */
	    tree index_type = c_common_signed_type (sizetype);

	    array_ptr_quals = declarator->u.array.quals;
	    array_ptr_attrs = declarator->u.array.attrs;
	    array_parm_static = declarator->u.array.static_p;
	    array_parm_vla_unspec_p = declarator->u.array.vla_unspec_p;

	    declarator = declarator->declarator;

	    /* Check for some types that there cannot be arrays of.  */

	    if (VOID_TYPE_P (type))
	      {
		if (name)
		  error_at (loc, "declaration of %qE as array of voids", name);
		else
		  error_at (loc, "declaration of type name as array of voids");
		type = error_mark_node;
	      }

	    if (TREE_CODE (type) == FUNCTION_TYPE)
	      {
		if (name)
		  error_at (loc, "declaration of %qE as array of functions",
		      	    name);
		else
		  error_at (loc, "declaration of type name as array of "
		            "functions");
		type = error_mark_node;
	      }

	    if (pedantic && !in_system_header_at (input_location)
		&& flexible_array_type_p (type))
	      pedwarn (loc, OPT_Wpedantic,
		       "invalid use of structure with flexible array member");

	    if (size == error_mark_node)
	      type = error_mark_node;

	    if (type == error_mark_node)
	      continue;

	    /* If size was specified, set ITYPE to a range-type for
	       that size.  Otherwise, ITYPE remains null.  finish_decl
	       may figure it out from an initial value.  */

	    if (size)
	      {
		bool size_maybe_const = true;
		bool size_int_const = (TREE_CODE (size) == INTEGER_CST
				       && !TREE_OVERFLOW (size));
		bool this_size_varies = false;

		/* Strip NON_LVALUE_EXPRs since we aren't using as an
		   lvalue.  */
		STRIP_TYPE_NOPS (size);

		if (!INTEGRAL_TYPE_P (TREE_TYPE (size)))
		  {
		    if (name)
		      error_at (loc, "size of array %qE has non-integer type",
				name);
		    else
		      error_at (loc,
				"size of unnamed array has non-integer type");
		    size = integer_one_node;
		  }
		/* This can happen with enum forward declaration.  */
		else if (!COMPLETE_TYPE_P (TREE_TYPE (size)))
		  {
		    if (name)
		      error_at (loc, "size of array %qE has incomplete type",
				name);
		    else
		      error_at (loc, "size of unnamed array has incomplete "
				"type");
		    size = integer_one_node;
		  }

		size = c_fully_fold (size, false, &size_maybe_const);

		if (pedantic && size_maybe_const && integer_zerop (size))
		  {
		    if (name)
		      pedwarn (loc, OPT_Wpedantic,
			       "ISO C forbids zero-size array %qE", name);
		    else
		      pedwarn (loc, OPT_Wpedantic,
			       "ISO C forbids zero-size array");
		  }

		if (TREE_CODE (size) == INTEGER_CST && size_maybe_const)
		  {
		    constant_expression_warning (size);
		    if (tree_int_cst_sgn (size) < 0)
		      {
			if (name)
			  error_at (loc, "size of array %qE is negative", name);
			else
			  error_at (loc, "size of unnamed array is negative");
			size = integer_one_node;
		      }
		    /* Handle a size folded to an integer constant but
		       not an integer constant expression.  */
		    if (!size_int_const)
		      {
			/* If this is a file scope declaration of an
			   ordinary identifier, this is invalid code;
			   diagnosing it here and not subsequently
			   treating the type as variable-length avoids
			   more confusing diagnostics later.  */
			if ((decl_context == NORMAL || decl_context == FIELD)
			    && current_scope == file_scope)
			  pedwarn (input_location, 0,
				   "variably modified %qE at file scope",
				   name);
			else
			  this_size_varies = size_varies = true;
			warn_variable_length_array (name, size);
		      }
		  }
		else if ((decl_context == NORMAL || decl_context == FIELD)
			 && current_scope == file_scope)
		  {
		    error_at (loc, "variably modified %qE at file scope", name);
		    size = integer_one_node;
		  }
		else
		  {
		    /* Make sure the array size remains visibly
		       nonconstant even if it is (eg) a const variable
		       with known value.  */
		    this_size_varies = size_varies = true;
		    warn_variable_length_array (name, size);
		    if (sanitize_flags_p (SANITIZE_VLA)
			&& current_function_decl != NULL_TREE
			&& decl_context == NORMAL)
		      {
			/* Evaluate the array size only once.  */
			size = save_expr (size);
			size = c_fully_fold (size, false, NULL);
		        size = fold_build2 (COMPOUND_EXPR, TREE_TYPE (size),
					    ubsan_instrument_vla (loc, size),
					    size);
		      }
		  }

		if (integer_zerop (size) && !this_size_varies)
		  {
		    /* A zero-length array cannot be represented with
		       an unsigned index type, which is what we'll
		       get with build_index_type.  Create an
		       open-ended range instead.  */
		    itype = build_range_type (sizetype, size, NULL_TREE);
		  }
		else
		  {
		    /* Arrange for the SAVE_EXPR on the inside of the
		       MINUS_EXPR, which allows the -1 to get folded
		       with the +1 that happens when building TYPE_SIZE.  */
		    if (size_varies)
		      size = save_expr (size);
		    if (this_size_varies && TREE_CODE (size) == INTEGER_CST)
		      size = build2 (COMPOUND_EXPR, TREE_TYPE (size),
				     integer_zero_node, size);

		    /* Compute the maximum valid index, that is, size
		       - 1.  Do the calculation in index_type, so that
		       if it is a variable the computations will be
		       done in the proper mode.  */
		    itype = fold_build2_loc (loc, MINUS_EXPR, index_type,
					     convert (index_type, size),
					     convert (index_type,
						      size_one_node));

		    /* The above overflows when size does not fit
		       in index_type.
		       ???  While a size of INT_MAX+1 technically shouldn't
		       cause an overflow (because we subtract 1), handling
		       this case seems like an unnecessary complication.  */
		    if (TREE_CODE (size) == INTEGER_CST
			&& !int_fits_type_p (size, index_type))
		      {
			if (name)
			  error_at (loc, "size of array %qE is too large",
			            name);
			else
			  error_at (loc, "size of unnamed array is too large");
			type = error_mark_node;
			continue;
		      }

		    itype = build_index_type (itype);
		  }
		if (this_size_varies)
		  {
		    if (*expr)
		      *expr = build2 (COMPOUND_EXPR, TREE_TYPE (size),
				      *expr, size);
		    else
		      *expr = size;
		    *expr_const_operands &= size_maybe_const;
		  }
	      }
	    else if (decl_context == FIELD)
	      {
		bool flexible_array_member = false;
		if (array_parm_vla_unspec_p)
		  /* Field names can in fact have function prototype
		     scope so [*] is disallowed here through making
		     the field variably modified, not through being
		     something other than a declaration with function
		     prototype scope.  */
		  size_varies = true;
		else
		  {
		    const struct c_declarator *t = declarator;
		    while (t->kind == cdk_attrs)
		      t = t->declarator;
		    flexible_array_member = (t->kind == cdk_id);
		  }
		if (flexible_array_member
		    && !in_system_header_at (input_location))
		  pedwarn_c90 (loc, OPT_Wpedantic, "ISO C90 does not "
			       "support flexible array members");

		/* ISO C99 Flexible array members are effectively
		   identical to GCC's zero-length array extension.  */
		if (flexible_array_member || array_parm_vla_unspec_p)
		  itype = build_range_type (sizetype, size_zero_node,
					    NULL_TREE);
	      }
	    else if (decl_context == PARM)
	      {
		if (array_parm_vla_unspec_p)
		  {
		    itype = build_range_type (sizetype, size_zero_node, NULL_TREE);
		    size_varies = true;
		  }
	      }
	    else if (decl_context == TYPENAME)
	      {
		if (array_parm_vla_unspec_p)
		  {
		    /* C99 6.7.5.2p4 */
		    warning (0, "%<[*]%> not in a declaration");
		    /* We use this to avoid messing up with incomplete
		       array types of the same type, that would
		       otherwise be modified below.  */
		    itype = build_range_type (sizetype, size_zero_node,
					      NULL_TREE);
		    size_varies = true;
		  }
	      }

	    /* Complain about arrays of incomplete types.  */
	    if (!COMPLETE_TYPE_P (type))
	      {
		error_at (loc, "array type has incomplete element type %qT",
			  type);
		/* See if we can be more helpful.  */
		if (TREE_CODE (type) == ARRAY_TYPE)
		  {
		    if (name)
		      inform (loc, "declaration of %qE as multidimensional "
			      "array must have bounds for all dimensions "
			      "except the first", name);
		    else
		      inform (loc, "declaration of multidimensional array "
			      "must have bounds for all dimensions except "
			      "the first");
		  }
		type = error_mark_node;
	      }
	    else
	    /* When itype is NULL, a shared incomplete array type is
	       returned for all array of a given type.  Elsewhere we
	       make sure we don't complete that type before copying
	       it, but here we want to make sure we don't ever
	       modify the shared type, so we gcc_assert (itype)
	       below.  */
	      {
		addr_space_t as = DECODE_QUAL_ADDR_SPACE (type_quals);
		if (!ADDR_SPACE_GENERIC_P (as) && as != TYPE_ADDR_SPACE (type))
		  type = build_qualified_type (type,
					       ENCODE_QUAL_ADDR_SPACE (as));

		type = build_array_type (type, itype);
	      }

	    if (type != error_mark_node)
	      {
		if (size_varies)
		  {
		    /* It is ok to modify type here even if itype is
		       NULL: if size_varies, we're in a
		       multi-dimensional array and the inner type has
		       variable size, so the enclosing shared array type
		       must too.  */
		    if (size && TREE_CODE (size) == INTEGER_CST)
		      type
			= build_distinct_type_copy (TYPE_MAIN_VARIANT (type));
		    C_TYPE_VARIABLE_SIZE (type) = 1;
		  }

		/* The GCC extension for zero-length arrays differs from
		   ISO flexible array members in that sizeof yields
		   zero.  */
		if (size && integer_zerop (size))
		  {
		    gcc_assert (itype);
		    type = build_distinct_type_copy (TYPE_MAIN_VARIANT (type));
		    TYPE_SIZE (type) = bitsize_zero_node;
		    TYPE_SIZE_UNIT (type) = size_zero_node;
		    SET_TYPE_STRUCTURAL_EQUALITY (type);
		  }
		if (array_parm_vla_unspec_p)
		  {
		    gcc_assert (itype);
		    /* The type is complete.  C99 6.7.5.2p4  */
		    type = build_distinct_type_copy (TYPE_MAIN_VARIANT (type));
		    TYPE_SIZE (type) = bitsize_zero_node;
		    TYPE_SIZE_UNIT (type) = size_zero_node;
		    SET_TYPE_STRUCTURAL_EQUALITY (type);
		  }

		if (!valid_array_size_p (loc, type, name))
		  type = error_mark_node;
	      }

	    if (decl_context != PARM
		&& (array_ptr_quals != TYPE_UNQUALIFIED
		    || array_ptr_attrs != NULL_TREE
		    || array_parm_static))
	      {
		error_at (loc, "static or type qualifiers in non-parameter "
			  "array declarator");
		array_ptr_quals = TYPE_UNQUALIFIED;
		array_ptr_attrs = NULL_TREE;
		array_parm_static = false;
	      }
	    orig_qual_indirect++;
	    break;
	  }
	case cdk_function:
	  {
	    /* Say it's a definition only for the declarator closest
	       to the identifier, apart possibly from some
	       attributes.  */
	    bool really_funcdef = false;
	    tree arg_types;
	    orig_qual_type = NULL_TREE;
	    if (funcdef_flag)
	      {
		const struct c_declarator *t = declarator->declarator;
		while (t->kind == cdk_attrs)
		  t = t->declarator;
		really_funcdef = (t->kind == cdk_id);
	      }

	    /* Declaring a function type.  Make sure we have a valid
	       type for the function to return.  */
	    if (type == error_mark_node)
	      continue;

	    size_varies = false;

	    /* Warn about some types functions can't return.  */
	    if (TREE_CODE (type) == FUNCTION_TYPE)
	      {
		if (name)
		  error_at (loc, "%qE declared as function returning a "
		      		 "function", name);
		else
		  error_at (loc, "type name declared as function "
			    "returning a function");
		type = integer_type_node;
	      }
	    if (TREE_CODE (type) == ARRAY_TYPE)
	      {
		if (name)
		  error_at (loc, "%qE declared as function returning an array",
		      	    name);
		else
		  error_at (loc, "type name declared as function returning "
		      	    "an array");
		type = integer_type_node;
	      }

	    /* Construct the function type and go to the next
	       inner layer of declarator.  */
	    arg_info = declarator->u.arg_info;
	    arg_types = grokparms (arg_info, really_funcdef);

	    /* Type qualifiers before the return type of the function
	       qualify the return type, not the function type.  */
	    if (type_quals)
	      {
		const enum c_declspec_word ignored_quals_list[] =
		  {
		    cdw_const, cdw_volatile, cdw_restrict, cdw_address_space,
		    cdw_atomic, cdw_number_of_elements
		  };
		location_t specs_loc
		  = smallest_type_quals_location (declspecs->locations,
						  ignored_quals_list);
		if (specs_loc == UNKNOWN_LOCATION)
		  specs_loc = declspecs->locations[cdw_typedef];
		if (specs_loc == UNKNOWN_LOCATION)
		  specs_loc = loc;

		/* Type qualifiers on a function return type are
		   normally permitted by the standard but have no
		   effect, so give a warning at -Wreturn-type.
		   Qualifiers on a void return type are banned on
		   function definitions in ISO C; GCC used to used
		   them for noreturn functions.  The resolution of C11
		   DR#423 means qualifiers (other than _Atomic) are
		   actually removed from the return type when
		   determining the function type.  */
		int quals_used = type_quals;
		if (flag_isoc11)
		  quals_used &= TYPE_QUAL_ATOMIC;
		if (quals_used && VOID_TYPE_P (type) && really_funcdef)
		  pedwarn (specs_loc, 0,
			   "function definition has qualified void return type");
		else
		  warning_at (specs_loc, OPT_Wignored_qualifiers,
			   "type qualifiers ignored on function return type");

		/* Ensure an error for restrict on invalid types; the
		   DR#423 resolution is not entirely clear about
		   this.  */
		if (flag_isoc11
		    && (type_quals & TYPE_QUAL_RESTRICT)
		    && (!POINTER_TYPE_P (type)
			|| !C_TYPE_OBJECT_OR_INCOMPLETE_P (TREE_TYPE (type))))
		  error_at (loc, "invalid use of %<restrict%>");
		if (quals_used)
		  type = c_build_qualified_type (type, quals_used);
	      }
	    type_quals = TYPE_UNQUALIFIED;

	    type = build_function_type (type, arg_types);
	    declarator = declarator->declarator;

	    /* Set the TYPE_CONTEXTs for each tagged type which is local to
	       the formal parameter list of this FUNCTION_TYPE to point to
	       the FUNCTION_TYPE node itself.  */
	    {
	      c_arg_tag *tag;
	      unsigned ix;

	      FOR_EACH_VEC_SAFE_ELT_REVERSE (arg_info->tags, ix, tag)
		TYPE_CONTEXT (tag->type) = type;
	    }
	    break;
	  }
	case cdk_pointer:
	  {
	    /* Merge any constancy or volatility into the target type
	       for the pointer.  */
	    if ((type_quals & TYPE_QUAL_ATOMIC)
		&& TREE_CODE (type) == FUNCTION_TYPE)
	      {
		error_at (loc,
			  "%<_Atomic%>-qualified function type");
		type_quals &= ~TYPE_QUAL_ATOMIC;
	      }
	    else if (pedantic && TREE_CODE (type) == FUNCTION_TYPE
		     && type_quals)
	      pedwarn (loc, OPT_Wpedantic,
		       "ISO C forbids qualified function types");
	    if (type_quals)
	      type = c_build_qualified_type (type, type_quals, orig_qual_type,
					     orig_qual_indirect);
	    orig_qual_type = NULL_TREE;
	    size_varies = false;

	    /* When the pointed-to type involves components of variable size,
	       care must be taken to ensure that the size evaluation code is
	       emitted early enough to dominate all the possible later uses
	       and late enough for the variables on which it depends to have
	       been assigned.

	       This is expected to happen automatically when the pointed-to
	       type has a name/declaration of it's own, but special attention
	       is required if the type is anonymous.

	       We handle the NORMAL and FIELD contexts here by attaching an
	       artificial TYPE_DECL to such pointed-to type.  This forces the
	       sizes evaluation at a safe point and ensures it is not deferred
	       until e.g. within a deeper conditional context.

	       We expect nothing to be needed here for PARM or TYPENAME.
	       Pushing a TYPE_DECL at this point for TYPENAME would actually
	       be incorrect, as we might be in the middle of an expression
	       with side effects on the pointed-to type size "arguments" prior
	       to the pointer declaration point and the fake TYPE_DECL in the
	       enclosing context would force the size evaluation prior to the
	       side effects.  */

	    if (!TYPE_NAME (type)
		&& (decl_context == NORMAL || decl_context == FIELD)
		&& variably_modified_type_p (type, NULL_TREE))
	      {
		tree decl = build_decl (loc, TYPE_DECL, NULL_TREE, type);
		DECL_ARTIFICIAL (decl) = 1;
		pushdecl (decl);
		finish_decl (decl, loc, NULL_TREE, NULL_TREE, NULL_TREE);
		TYPE_NAME (type) = decl;
	      }

	    type = c_build_pointer_type (type);

	    /* Process type qualifiers (such as const or volatile)
	       that were given inside the `*'.  */
	    type_quals = declarator->u.pointer_quals;

	    declarator = declarator->declarator;
	    break;
	  }
	default:
	  gcc_unreachable ();
	}
    }
  *decl_attrs = chainon (returned_attrs, *decl_attrs);

  /* Now TYPE has the actual type, apart from any qualifiers in
     TYPE_QUALS.  */

  /* Warn about address space used for things other than static memory or
     pointers.  */
  address_space = DECODE_QUAL_ADDR_SPACE (type_quals);
  if (!ADDR_SPACE_GENERIC_P (address_space))
    {
      if (decl_context == NORMAL)
	{
	  switch (storage_class)
	    {
	    case csc_auto:
	      error ("%qs combined with %<auto%> qualifier for %qE",
		     c_addr_space_name (address_space), name);
	      break;
	    case csc_register:
	      error ("%qs combined with %<register%> qualifier for %qE",
		     c_addr_space_name (address_space), name);
	      break;
	    case csc_none:
	      if (current_function_scope)
		{
		  error ("%qs specified for auto variable %qE",
			 c_addr_space_name (address_space), name);
		  break;
		}
	      break;
	    case csc_static:
	    case csc_extern:
	    case csc_typedef:
	      break;
	    default:
	      gcc_unreachable ();
	    }
	}
      else if (decl_context == PARM && TREE_CODE (type) != ARRAY_TYPE)
	{
	  if (name)
	    error ("%qs specified for parameter %qE",
		   c_addr_space_name (address_space), name);
	  else
	    error ("%qs specified for unnamed parameter",
		   c_addr_space_name (address_space));
	}
      else if (decl_context == FIELD)
	{
	  if (name)
	    error ("%qs specified for structure field %qE",
		   c_addr_space_name (address_space), name);
	  else
	    error ("%qs specified for structure field",
		   c_addr_space_name (address_space));
	}
    }

  /* Check the type and width of a bit-field.  */
  if (bitfield)
    {
      check_bitfield_type_and_width (loc, &type, width, name);
      /* C11 makes it implementation-defined (6.7.2.1#5) whether
	 atomic types are permitted for bit-fields; we have no code to
	 make bit-field accesses atomic, so disallow them.  */
      if (type_quals & TYPE_QUAL_ATOMIC)
	{
	  if (name)
	    error_at (loc, "bit-field %qE has atomic type", name);
	  else
	    error_at (loc, "bit-field has atomic type");
	  type_quals &= ~TYPE_QUAL_ATOMIC;
	}
    }

  /* Reject invalid uses of _Alignas.  */
  if (declspecs->alignas_p)
    {
      if (storage_class == csc_typedef)
	error_at (loc, "alignment specified for typedef %qE", name);
      else if (storage_class == csc_register)
	error_at (loc, "alignment specified for %<register%> object %qE",
		  name);
      else if (decl_context == PARM)
	{
	  if (name)
	    error_at (loc, "alignment specified for parameter %qE", name);
	  else
	    error_at (loc, "alignment specified for unnamed parameter");
	}
      else if (bitfield)
	{
	  if (name)
	    error_at (loc, "alignment specified for bit-field %qE", name);
	  else
	    error_at (loc, "alignment specified for unnamed bit-field");
	}
      else if (TREE_CODE (type) == FUNCTION_TYPE)
	error_at (loc, "alignment specified for function %qE", name);
      else if (declspecs->align_log != -1 && TYPE_P (type))
	{
	  alignas_align = 1U << declspecs->align_log;
	  if (alignas_align < min_align_of_type (type))
	    {
	      if (name)
		error_at (loc, "%<_Alignas%> specifiers cannot reduce "
			  "alignment of %qE", name);
	      else
		error_at (loc, "%<_Alignas%> specifiers cannot reduce "
			  "alignment of unnamed field");
	      alignas_align = 0;
	    }
	}
    }

  /* If this is declaring a typedef name, return a TYPE_DECL.  */

  if (storage_class == csc_typedef)
    {
      tree decl;
      if ((type_quals & TYPE_QUAL_ATOMIC)
	  && TREE_CODE (type) == FUNCTION_TYPE)
	{
	  error_at (loc,
		    "%<_Atomic%>-qualified function type");
	  type_quals &= ~TYPE_QUAL_ATOMIC;
	}
      else if (pedantic && TREE_CODE (type) == FUNCTION_TYPE
	       && type_quals)
	pedwarn (loc, OPT_Wpedantic,
		 "ISO C forbids qualified function types");
      if (type_quals)
	type = c_build_qualified_type (type, type_quals, orig_qual_type,
				       orig_qual_indirect);
      decl = build_decl (declarator->id_loc,
			 TYPE_DECL, declarator->u.id, type);
      if (declspecs->explicit_signed_p)
	C_TYPEDEF_EXPLICITLY_SIGNED (decl) = 1;
      if (declspecs->inline_p)
	pedwarn (loc, 0,"typedef %q+D declared %<inline%>", decl);
      if (declspecs->noreturn_p)
	pedwarn (loc, 0,"typedef %q+D declared %<_Noreturn%>", decl);

      if (warn_cxx_compat && declarator->u.id != NULL_TREE)
	{
	  struct c_binding *b = I_TAG_BINDING (declarator->u.id);

	  if (b != NULL
	      && b->decl != NULL_TREE
	      && (B_IN_CURRENT_SCOPE (b)
		  || (current_scope == file_scope && B_IN_EXTERNAL_SCOPE (b)))
	      && TYPE_MAIN_VARIANT (b->decl) != TYPE_MAIN_VARIANT (type))
	    {
	      if (warning_at (declarator->id_loc, OPT_Wc___compat,
			      ("using %qD as both a typedef and a tag is "
			       "invalid in C++"), decl)
		  && b->locus != UNKNOWN_LOCATION)
		inform (b->locus, "originally defined here");
	    }
	}

      return decl;
    }

  /* If this is a type name (such as, in a cast or sizeof),
     compute the type and return it now.  */

  if (decl_context == TYPENAME)
    {
      /* Note that the grammar rejects storage classes in typenames
	 and fields.  */
      gcc_assert (storage_class == csc_none && !threadp
		  && !declspecs->inline_p && !declspecs->noreturn_p);
      if ((type_quals & TYPE_QUAL_ATOMIC)
	  && TREE_CODE (type) == FUNCTION_TYPE)
	{
	  error_at (loc,
		    "%<_Atomic%>-qualified function type");
	  type_quals &= ~TYPE_QUAL_ATOMIC;
	}
      else if (pedantic && TREE_CODE (type) == FUNCTION_TYPE
	       && type_quals)
	pedwarn (loc, OPT_Wpedantic,
		 "ISO C forbids const or volatile function types");
      if (type_quals)
	type = c_build_qualified_type (type, type_quals, orig_qual_type,
				       orig_qual_indirect);
      return type;
    }

  if (pedantic && decl_context == FIELD
      && variably_modified_type_p (type, NULL_TREE))
    {
      /* C99 6.7.2.1p8 */
      pedwarn (loc, OPT_Wpedantic, "a member of a structure or union cannot "
	       "have a variably modified type");
    }

  /* Aside from typedefs and type names (handle above),
     `void' at top level (not within pointer)
     is allowed only in public variables.
     We don't complain about parms either, but that is because
     a better error message can be made later.  */

  if (VOID_TYPE_P (type) && decl_context != PARM
      && !((decl_context != FIELD && TREE_CODE (type) != FUNCTION_TYPE)
	    && (storage_class == csc_extern
		|| (current_scope == file_scope
		    && !(storage_class == csc_static
			 || storage_class == csc_register)))))
    {
      error_at (loc, "variable or field %qE declared void", name);
      type = integer_type_node;
    }

  /* Now create the decl, which may be a VAR_DECL, a PARM_DECL
     or a FUNCTION_DECL, depending on DECL_CONTEXT and TYPE.  */

  {
    tree decl;

    if (decl_context == PARM)
      {
	tree promoted_type;
	bool array_parameter_p = false;

	/* A parameter declared as an array of T is really a pointer to T.
	   One declared as a function is really a pointer to a function.  */

	if (TREE_CODE (type) == ARRAY_TYPE)
	  {
	    /* Transfer const-ness of array into that of type pointed to.  */
	    type = TREE_TYPE (type);
	    if (orig_qual_type != NULL_TREE)
	      {
		if (orig_qual_indirect == 0)
		  orig_qual_type = TREE_TYPE (orig_qual_type);
		else
		  orig_qual_indirect--;
	      }
	    if (type_quals)
	      type = c_build_qualified_type (type, type_quals, orig_qual_type,
					     orig_qual_indirect);
	    type = c_build_pointer_type (type);
	    type_quals = array_ptr_quals;
	    if (type_quals)
	      type = c_build_qualified_type (type, type_quals);

	    /* We don't yet implement attributes in this context.  */
	    if (array_ptr_attrs != NULL_TREE)
	      warning_at (loc, OPT_Wattributes,
			  "attributes in parameter array declarator ignored");

	    size_varies = false;
	    array_parameter_p = true;
	  }
	else if (TREE_CODE (type) == FUNCTION_TYPE)
	  {
	    if (type_quals & TYPE_QUAL_ATOMIC)
	      {
		error_at (loc,
			  "%<_Atomic%>-qualified function type");
		type_quals &= ~TYPE_QUAL_ATOMIC;
	      }
	    else if (type_quals)
	      pedwarn (loc, OPT_Wpedantic,
		       "ISO C forbids qualified function types");
	    if (type_quals)
	      type = c_build_qualified_type (type, type_quals);
	    type = c_build_pointer_type (type);
	    type_quals = TYPE_UNQUALIFIED;
	  }
	else if (type_quals)
	  type = c_build_qualified_type (type, type_quals);

	decl = build_decl (declarator->id_loc,
			   PARM_DECL, declarator->u.id, type);
	if (size_varies)
	  C_DECL_VARIABLE_SIZE (decl) = 1;
	C_ARRAY_PARAMETER (decl) = array_parameter_p;

	/* Compute the type actually passed in the parmlist,
	   for the case where there is no prototype.
	   (For example, shorts and chars are passed as ints.)
	   When there is a prototype, this is overridden later.  */

	if (type == error_mark_node)
	  promoted_type = type;
	else
	  promoted_type = c_type_promotes_to (type);

	DECL_ARG_TYPE (decl) = promoted_type;
	if (declspecs->inline_p)
	  pedwarn (loc, 0, "parameter %q+D declared %<inline%>", decl);
	if (declspecs->noreturn_p)
	  pedwarn (loc, 0, "parameter %q+D declared %<_Noreturn%>", decl);
      }
    else if (decl_context == FIELD)
      {
	/* Note that the grammar rejects storage classes in typenames
	   and fields.  */
	gcc_assert (storage_class == csc_none && !threadp
		    && !declspecs->inline_p && !declspecs->noreturn_p);

	/* Structure field.  It may not be a function.  */

	if (TREE_CODE (type) == FUNCTION_TYPE)
	  {
	    error_at (loc, "field %qE declared as a function", name);
	    type = build_pointer_type (type);
	  }
	else if (TREE_CODE (type) != ERROR_MARK
		 && !COMPLETE_OR_UNBOUND_ARRAY_TYPE_P (type))
	  {
	    if (name)
	      error_at (loc, "field %qE has incomplete type", name);
	    else
	      error_at (loc, "unnamed field has incomplete type");
	    type = error_mark_node;
	  }
	else if (TREE_CODE (type) == ARRAY_TYPE
		 && TYPE_DOMAIN (type) == NULL_TREE)
	  {
	    /* We have a flexible array member through a typedef.
	       Set suitable range.  Whether this is a correct position
	       for a flexible array member will be determined elsewhere.  */
	    if (!in_system_header_at (input_location))
	      pedwarn_c90 (loc, OPT_Wpedantic, "ISO C90 does not "
			   "support flexible array members");
	    type = build_distinct_type_copy (TYPE_MAIN_VARIANT (type));
	    TYPE_DOMAIN (type) = build_range_type (sizetype, size_zero_node,
						   NULL_TREE);
	    if (orig_qual_indirect == 0)
	      orig_qual_type = NULL_TREE;
	  }
	type = c_build_qualified_type (type, type_quals, orig_qual_type,
				       orig_qual_indirect);
	decl = build_decl (declarator->id_loc,
			   FIELD_DECL, declarator->u.id, type);
	DECL_NONADDRESSABLE_P (decl) = bitfield;
	if (bitfield && !declarator->u.id)
	  TREE_NO_WARNING (decl) = 1;

	if (size_varies)
	  C_DECL_VARIABLE_SIZE (decl) = 1;
      }
    else if (TREE_CODE (type) == FUNCTION_TYPE)
      {
	if (storage_class == csc_register || threadp)
	  {
	    error_at (loc, "invalid storage class for function %qE", name);
	  }
	else if (current_scope != file_scope)
	  {
	    /* Function declaration not at file scope.  Storage
	       classes other than `extern' are not allowed, C99
	       6.7.1p5, and `extern' makes no difference.  However,
	       GCC allows 'auto', perhaps with 'inline', to support
	       nested functions.  */
	    if (storage_class == csc_auto)
		pedwarn (loc, OPT_Wpedantic,
			 "invalid storage class for function %qE", name);
	    else if (storage_class == csc_static)
	      {
		error_at (loc, "invalid storage class for function %qE", name);
		if (funcdef_flag)
		  storage_class = declspecs->storage_class = csc_none;
		else
		  return NULL_TREE;
	      }
	  }

	decl = build_decl (declarator->id_loc,
			   FUNCTION_DECL, declarator->u.id, type);
	decl = build_decl_attribute_variant (decl, decl_attr);

	if (type_quals & TYPE_QUAL_ATOMIC)
	  {
	    error_at (loc,
		      "%<_Atomic%>-qualified function type");
	    type_quals &= ~TYPE_QUAL_ATOMIC;
	  }
	else if (pedantic && type_quals && !DECL_IN_SYSTEM_HEADER (decl))
	  pedwarn (loc, OPT_Wpedantic,
		   "ISO C forbids qualified function types");

	/* Every function declaration is an external reference
	   (DECL_EXTERNAL) except for those which are not at file
	   scope and are explicitly declared "auto".  This is
	   forbidden by standard C (C99 6.7.1p5) and is interpreted by
	   GCC to signify a forward declaration of a nested function.  */
	if (storage_class == csc_auto && current_scope != file_scope)
	  DECL_EXTERNAL (decl) = 0;
	/* In C99, a function which is declared 'inline' with 'extern'
	   is not an external reference (which is confusing).  It
	   means that the later definition of the function must be output
	   in this file, C99 6.7.4p6.  In GNU C89, a function declared
	   'extern inline' is an external reference.  */
	else if (declspecs->inline_p && storage_class != csc_static)
	  DECL_EXTERNAL (decl) = ((storage_class == csc_extern)
				  == flag_gnu89_inline);
	else
	  DECL_EXTERNAL (decl) = !initialized;

	/* Record absence of global scope for `static' or `auto'.  */
	TREE_PUBLIC (decl)
	  = !(storage_class == csc_static || storage_class == csc_auto);

	/* For a function definition, record the argument information
	   block where store_parm_decls will look for it.  */
	if (funcdef_flag)
	  current_function_arg_info = arg_info;

	if (declspecs->default_int_p)
	  C_FUNCTION_IMPLICIT_INT (decl) = 1;

	/* Record presence of `inline' and `_Noreturn', if it is
	   reasonable.  */
	if (flag_hosted && MAIN_NAME_P (declarator->u.id))
	  {
	    if (declspecs->inline_p)
	      pedwarn (loc, 0, "cannot inline function %<main%>");
	    if (declspecs->noreturn_p)
	      pedwarn (loc, 0, "%<main%> declared %<_Noreturn%>");
	  }
	else
	  {
	    if (declspecs->inline_p)
	      /* Record that the function is declared `inline'.  */
	      DECL_DECLARED_INLINE_P (decl) = 1;
	    if (declspecs->noreturn_p)
	      {
		if (flag_isoc99)
		  pedwarn_c99 (loc, OPT_Wpedantic,
			       "ISO C99 does not support %<_Noreturn%>");
		else
		  pedwarn_c99 (loc, OPT_Wpedantic,
			       "ISO C90 does not support %<_Noreturn%>");
		TREE_THIS_VOLATILE (decl) = 1;
	      }
	  }
      }
    else
      {
	/* It's a variable.  */
	/* An uninitialized decl with `extern' is a reference.  */
	int extern_ref = !initialized && storage_class == csc_extern;

	type = c_build_qualified_type (type, type_quals, orig_qual_type,
				       orig_qual_indirect);

	/* C99 6.2.2p7: It is invalid (compile-time undefined
	   behavior) to create an 'extern' declaration for a
	   variable if there is a global declaration that is
	   'static' and the global declaration is not visible.
	   (If the static declaration _is_ currently visible,
	   the 'extern' declaration is taken to refer to that decl.) */
	if (extern_ref && current_scope != file_scope)
	  {
	    tree global_decl  = identifier_global_value (declarator->u.id);
	    tree visible_decl = lookup_name (declarator->u.id);

	    if (global_decl
		&& global_decl != visible_decl
		&& VAR_P (global_decl)
		&& !TREE_PUBLIC (global_decl))
	      error_at (loc, "variable previously declared %<static%> "
			"redeclared %<extern%>");
	  }

	decl = build_decl (declarator->id_loc,
			   VAR_DECL, declarator->u.id, type);
	if (size_varies)
	  C_DECL_VARIABLE_SIZE (decl) = 1;

	if (declspecs->inline_p)
	  pedwarn (loc, 0, "variable %q+D declared %<inline%>", decl);
	if (declspecs->noreturn_p)
	  pedwarn (loc, 0, "variable %q+D declared %<_Noreturn%>", decl);

	/* At file scope, an initialized extern declaration may follow
	   a static declaration.  In that case, DECL_EXTERNAL will be
	   reset later in start_decl.  */
	DECL_EXTERNAL (decl) = (storage_class == csc_extern);

	/* At file scope, the presence of a `static' or `register' storage
	   class specifier, or the absence of all storage class specifiers
	   makes this declaration a definition (perhaps tentative).  Also,
	   the absence of `static' makes it public.  */
	if (current_scope == file_scope)
	  {
	    TREE_PUBLIC (decl) = storage_class != csc_static;
	    TREE_STATIC (decl) = !extern_ref;
	  }
	/* Not at file scope, only `static' makes a static definition.  */
	else
	  {
	    TREE_STATIC (decl) = (storage_class == csc_static);
	    TREE_PUBLIC (decl) = extern_ref;
	  }

	if (threadp)
	  set_decl_tls_model (decl, decl_default_tls_model (decl));
      }

    if ((storage_class == csc_extern
	 || (storage_class == csc_none
	     && TREE_CODE (type) == FUNCTION_TYPE
	     && !funcdef_flag))
	&& variably_modified_type_p (type, NULL_TREE))
      {
	/* C99 6.7.5.2p2 */
	if (TREE_CODE (type) == FUNCTION_TYPE)
	  error_at (loc, "non-nested function with variably modified type");
	else
	  error_at (loc, "object with variably modified type must have "
	      	    "no linkage");
      }

    /* Record `register' declaration for warnings on &
       and in case doing stupid register allocation.  */

    if (storage_class == csc_register)
      {
	C_DECL_REGISTER (decl) = 1;
	DECL_REGISTER (decl) = 1;
      }

    /* Record constancy and volatility.  */
    c_apply_type_quals_to_decl (type_quals, decl);

    /* Apply _Alignas specifiers.  */
    if (alignas_align)
      {
	SET_DECL_ALIGN (decl, alignas_align * BITS_PER_UNIT);
	DECL_USER_ALIGN (decl) = 1;
      }

    /* If a type has volatile components, it should be stored in memory.
       Otherwise, the fact that those components are volatile
       will be ignored, and would even crash the compiler.
       Of course, this only makes sense on  VAR,PARM, and RESULT decl's.   */
    if (C_TYPE_FIELDS_VOLATILE (TREE_TYPE (decl))
	&& (VAR_P (decl) ||  TREE_CODE (decl) == PARM_DECL
	  || TREE_CODE (decl) == RESULT_DECL))
      {
	/* It is not an error for a structure with volatile fields to
	   be declared register, but reset DECL_REGISTER since it
	   cannot actually go in a register.  */
	int was_reg = C_DECL_REGISTER (decl);
	C_DECL_REGISTER (decl) = 0;
	DECL_REGISTER (decl) = 0;
	c_mark_addressable (decl);
	C_DECL_REGISTER (decl) = was_reg;
      }

  /* This is the earliest point at which we might know the assembler
     name of a variable.  Thus, if it's known before this, die horribly.  */
    gcc_assert (!HAS_DECL_ASSEMBLER_NAME_P (decl)
		|| !DECL_ASSEMBLER_NAME_SET_P (decl));

    if (warn_cxx_compat
	&& VAR_P (decl)
	&& TREE_PUBLIC (decl)
	&& TREE_STATIC (decl)
	&& (RECORD_OR_UNION_TYPE_P (TREE_TYPE (decl))
	    || TREE_CODE (TREE_TYPE (decl)) == ENUMERAL_TYPE)
	&& TYPE_NAME (TREE_TYPE (decl)) == NULL_TREE)
      warning_at (DECL_SOURCE_LOCATION (decl), OPT_Wc___compat,
		  ("non-local variable %qD with anonymous type is "
		   "questionable in C++"),
		  decl);

    return decl;
  }
}

/* Decode the parameter-list info for a function type or function definition.
   The argument is the value returned by `get_parm_info' (or made in c-parse.c
   if there is an identifier list instead of a parameter decl list).
   These two functions are separate because when a function returns
   or receives functions then each is called multiple times but the order
   of calls is different.  The last call to `grokparms' is always the one
   that contains the formal parameter names of a function definition.

   Return a list of arg types to use in the FUNCTION_TYPE for this function.

   FUNCDEF_FLAG is true for a function definition, false for
   a mere declaration.  A nonempty identifier-list gets an error message
   when FUNCDEF_FLAG is false.  */

static tree
grokparms (struct c_arg_info *arg_info, bool funcdef_flag)
{
  tree arg_types = arg_info->types;

  if (funcdef_flag && arg_info->had_vla_unspec)
    {
      /* A function definition isn't function prototype scope C99 6.2.1p4.  */
      /* C99 6.7.5.2p4 */
      error ("%<[*]%> not allowed in other than function prototype scope");
    }

  if (arg_types == NULL_TREE && !funcdef_flag
      && !in_system_header_at (input_location))
    warning (OPT_Wstrict_prototypes,
	     "function declaration isn%'t a prototype");

  if (arg_types == error_mark_node)
    /* Don't set TYPE_ARG_TYPES in this case.  */
    return NULL_TREE;

  else if (arg_types && TREE_CODE (TREE_VALUE (arg_types)) == IDENTIFIER_NODE)
    {
      if (!funcdef_flag)
	{
	  pedwarn (input_location, 0, "parameter names (without types) in "
		   "function declaration");
	  arg_info->parms = NULL_TREE;
	}
      else
	arg_info->parms = arg_info->types;

      arg_info->types = NULL_TREE;
      return NULL_TREE;
    }
  else
    {
      tree parm, type, typelt;
      unsigned int parmno;

      /* If there is a parameter of incomplete type in a definition,
	 this is an error.  In a declaration this is valid, and a
	 struct or union type may be completed later, before any calls
	 or definition of the function.  In the case where the tag was
	 first declared within the parameter list, a warning has
	 already been given.  If a parameter has void type, then
	 however the function cannot be defined or called, so
	 warn.  */

      for (parm = arg_info->parms, typelt = arg_types, parmno = 1;
	   parm;
	   parm = DECL_CHAIN (parm), typelt = TREE_CHAIN (typelt), parmno++)
	{
	  type = TREE_VALUE (typelt);
	  if (type == error_mark_node)
	    continue;

	  if (!COMPLETE_TYPE_P (type))
	    {
	      if (funcdef_flag)
		{
		  if (DECL_NAME (parm))
		    error_at (input_location,
			      "parameter %u (%q+D) has incomplete type",
			      parmno, parm);
		  else
		    error_at (DECL_SOURCE_LOCATION (parm),
			      "parameter %u has incomplete type",
			      parmno);

		  TREE_VALUE (typelt) = error_mark_node;
		  TREE_TYPE (parm) = error_mark_node;
		  arg_types = NULL_TREE;
		}
	      else if (VOID_TYPE_P (type))
		{
		  if (DECL_NAME (parm))
		    warning_at (input_location, 0,
				"parameter %u (%q+D) has void type",
				parmno, parm);
		  else
		    warning_at (DECL_SOURCE_LOCATION (parm), 0,
				"parameter %u has void type",
				parmno);
		}
	    }

	  if (DECL_NAME (parm) && TREE_USED (parm))
	    warn_if_shadowing (parm);
	}
      return arg_types;
    }
}

/* Allocate and initialize a c_arg_info structure from the parser's
   obstack.  */

struct c_arg_info *
build_arg_info (void)
{
  struct c_arg_info *ret = XOBNEW (&parser_obstack, struct c_arg_info);
  ret->parms = NULL_TREE;
  ret->tags = NULL;
  ret->types = NULL_TREE;
  ret->others = NULL_TREE;
  ret->pending_sizes = NULL;
  ret->had_vla_unspec = 0;
  return ret;
}

/* Take apart the current scope and return a c_arg_info structure with
   info on a parameter list just parsed.

   This structure is later fed to 'grokparms' and 'store_parm_decls'.

   ELLIPSIS being true means the argument list ended in '...' so don't
   append a sentinel (void_list_node) to the end of the type-list.

   EXPR is NULL or an expression that needs to be evaluated for the
   side effects of array size expressions in the parameters.  */

struct c_arg_info *
get_parm_info (bool ellipsis, tree expr)
{
  struct c_binding *b = current_scope->bindings;
  struct c_arg_info *arg_info = build_arg_info ();

  tree parms = NULL_TREE;
  vec<c_arg_tag, va_gc> *tags = NULL;
  tree types = NULL_TREE;
  tree others = NULL_TREE;

  bool gave_void_only_once_err = false;

  arg_info->had_vla_unspec = current_scope->had_vla_unspec;

  /* The bindings in this scope must not get put into a block.
     We will take care of deleting the binding nodes.  */
  current_scope->bindings = 0;

  /* This function is only called if there was *something* on the
     parameter list.  */
  gcc_assert (b);

  /* A parameter list consisting solely of 'void' indicates that the
     function takes no arguments.  But if the 'void' is qualified
     (by 'const' or 'volatile'), or has a storage class specifier
     ('register'), then the behavior is undefined; issue an error.
     Typedefs for 'void' are OK (see DR#157).  */
  if (b->prev == 0			    /* one binding */
      && TREE_CODE (b->decl) == PARM_DECL   /* which is a parameter */
      && !DECL_NAME (b->decl)               /* anonymous */
      && VOID_TYPE_P (TREE_TYPE (b->decl))) /* of void type */
    {
      if (TYPE_QUALS (TREE_TYPE (b->decl)) != TYPE_UNQUALIFIED
	  || C_DECL_REGISTER (b->decl))
	error_at (b->locus, "%<void%> as only parameter may not be qualified");

      /* There cannot be an ellipsis.  */
      if (ellipsis)
	error_at (b->locus, "%<void%> must be the only parameter");

      arg_info->types = void_list_node;
      return arg_info;
    }

  if (!ellipsis)
    types = void_list_node;

  /* Break up the bindings list into parms, tags, types, and others;
     apply sanity checks; purge the name-to-decl bindings.  */
  while (b)
    {
      tree decl = b->decl;
      tree type = TREE_TYPE (decl);
      c_arg_tag tag;
      const char *keyword;

      switch (TREE_CODE (decl))
	{
	case PARM_DECL:
	  if (b->id)
	    {
	      gcc_assert (I_SYMBOL_BINDING (b->id) == b);
	      I_SYMBOL_BINDING (b->id) = b->shadowed;
	    }

	  /* Check for forward decls that never got their actual decl.  */
	  if (TREE_ASM_WRITTEN (decl))
	    error_at (b->locus,
		      "parameter %q+D has just a forward declaration", decl);
	  /* Check for (..., void, ...) and issue an error.  */
	  else if (VOID_TYPE_P (type) && !DECL_NAME (decl))
	    {
	      if (!gave_void_only_once_err)
		{
		  error_at (b->locus, "%<void%> must be the only parameter");
		  gave_void_only_once_err = true;
		}
	    }
	  else
	    {
	      /* Valid parameter, add it to the list.  */
	      DECL_CHAIN (decl) = parms;
	      parms = decl;

	      /* Since there is a prototype, args are passed in their
		 declared types.  The back end may override this later.  */
	      DECL_ARG_TYPE (decl) = type;
	      types = tree_cons (0, type, types);
	    }
	  break;

	case ENUMERAL_TYPE: keyword = "enum"; goto tag;
	case UNION_TYPE:    keyword = "union"; goto tag;
	case RECORD_TYPE:   keyword = "struct"; goto tag;
	tag:
	  /* Types may not have tag-names, in which case the type
	     appears in the bindings list with b->id NULL.  */
	  if (b->id)
	    {
	      gcc_assert (I_TAG_BINDING (b->id) == b);
	      I_TAG_BINDING (b->id) = b->shadowed;
	    }

	  /* Warn about any struct, union or enum tags defined in a
	     parameter list.  The scope of such types is limited to
	     the parameter list, which is rarely if ever desirable
	     (it's impossible to call such a function with type-
	     correct arguments).  An anonymous union parm type is
	     meaningful as a GNU extension, so don't warn for that.  */
	  if (TREE_CODE (decl) != UNION_TYPE || b->id != NULL_TREE)
	    {
	      if (b->id)
		/* The %s will be one of 'struct', 'union', or 'enum'.  */
		warning_at (b->locus, 0,
			    "%<%s %E%> declared inside parameter list"
			    " will not be visible outside of this definition or"
			    " declaration", keyword, b->id);
	      else
		/* The %s will be one of 'struct', 'union', or 'enum'.  */
		warning_at (b->locus, 0,
			    "anonymous %s declared inside parameter list"
			    " will not be visible outside of this definition or"
			    " declaration", keyword);
	    }

	  tag.id = b->id;
	  tag.type = decl;
	  vec_safe_push (tags, tag);
	  break;

	case FUNCTION_DECL:
	  /* FUNCTION_DECLs appear when there is an implicit function
	     declaration in the parameter list.  */
	  gcc_assert (b->nested || seen_error ());
	  goto set_shadowed;

	case CONST_DECL:
	case TYPE_DECL:
	  /* CONST_DECLs appear here when we have an embedded enum,
	     and TYPE_DECLs appear here when we have an embedded struct
	     or union.  No warnings for this - we already warned about the
	     type itself.  */

	  /* When we reinsert this decl in the function body, we need
	     to reconstruct whether it was marked as nested.  */
	  gcc_assert (!b->nested);
	  DECL_CHAIN (decl) = others;
	  others = decl;
	  /* fall through */

	case ERROR_MARK:
	set_shadowed:
	  /* error_mark_node appears here when we have an undeclared
	     variable.  Just throw it away.  */
	  if (b->id)
	    {
	      gcc_assert (I_SYMBOL_BINDING (b->id) == b);
	      I_SYMBOL_BINDING (b->id) = b->shadowed;
	    }
	  break;

	  /* Other things that might be encountered.  */
	case LABEL_DECL:
	case VAR_DECL:
	default:
	  gcc_unreachable ();
	}

      b = free_binding_and_advance (b);
    }

  arg_info->parms = parms;
  arg_info->tags = tags;
  arg_info->types = types;
  arg_info->others = others;
  arg_info->pending_sizes = expr;
  return arg_info;
}

/* Get the struct, enum or union (CODE says which) with tag NAME.
   Define the tag as a forward-reference with location LOC if it is
   not defined.  Return a c_typespec structure for the type
   specifier.  */

struct c_typespec
parser_xref_tag (location_t loc, enum tree_code code, tree name)
{
  struct c_typespec ret;
  tree ref;
  location_t refloc;

  ret.expr = NULL_TREE;
  ret.expr_const_operands = true;

  /* If a cross reference is requested, look up the type
     already defined for this tag and return it.  */

  ref = lookup_tag (code, name, false, &refloc);
  /* If this is the right type of tag, return what we found.
     (This reference will be shadowed by shadow_tag later if appropriate.)
     If this is the wrong type of tag, do not return it.  If it was the
     wrong type in the same scope, we will have had an error
     message already; if in a different scope and declaring
     a name, pending_xref_error will give an error message; but if in a
     different scope and not declaring a name, this tag should
     shadow the previous declaration of a different type of tag, and
     this would not work properly if we return the reference found.
     (For example, with "struct foo" in an outer scope, "union foo;"
     must shadow that tag with a new one of union type.)  */
  ret.kind = (ref ? ctsk_tagref : ctsk_tagfirstref);
  if (ref && TREE_CODE (ref) == code)
    {
      if (C_TYPE_DEFINED_IN_STRUCT (ref)
	  && loc != UNKNOWN_LOCATION
	  && warn_cxx_compat)
	{
	  switch (code)
	    {
	    case ENUMERAL_TYPE:
	      warning_at (loc, OPT_Wc___compat,
			  ("enum type defined in struct or union "
			   "is not visible in C++"));
	      inform (refloc, "enum type defined here");
	      break;
	    case RECORD_TYPE:
	      warning_at (loc, OPT_Wc___compat,
			  ("struct defined in struct or union "
			   "is not visible in C++"));
	      inform (refloc, "struct defined here");
	      break;
	    case UNION_TYPE:
	      warning_at (loc, OPT_Wc___compat,
			  ("union defined in struct or union "
			   "is not visible in C++"));
	      inform (refloc, "union defined here");
	      break;
	    default:
	      gcc_unreachable();
	    }
	}

      ret.spec = ref;
      return ret;
    }

  /* If no such tag is yet defined, create a forward-reference node
     and record it as the "definition".
     When a real declaration of this type is found,
     the forward-reference will be altered into a real type.  */

  ref = make_node (code);
  if (code == ENUMERAL_TYPE)
    {
      /* Give the type a default layout like unsigned int
	 to avoid crashing if it does not get defined.  */
      SET_TYPE_MODE (ref, TYPE_MODE (unsigned_type_node));
      SET_TYPE_ALIGN (ref, TYPE_ALIGN (unsigned_type_node));
      TYPE_USER_ALIGN (ref) = 0;
      TYPE_UNSIGNED (ref) = 1;
      TYPE_PRECISION (ref) = TYPE_PRECISION (unsigned_type_node);
      TYPE_MIN_VALUE (ref) = TYPE_MIN_VALUE (unsigned_type_node);
      TYPE_MAX_VALUE (ref) = TYPE_MAX_VALUE (unsigned_type_node);
    }

  pushtag (loc, name, ref);

  ret.spec = ref;
  return ret;
}

/* Get the struct, enum or union (CODE says which) with tag NAME.
   Define the tag as a forward-reference if it is not defined.
   Return a tree for the type.  */

tree
xref_tag (enum tree_code code, tree name)
{
  return parser_xref_tag (input_location, code, name).spec;
}

/* Make sure that the tag NAME is defined *in the current scope*
   at least as a forward reference.
   LOC is the location of the struct's definition.
   CODE says which kind of tag NAME ought to be.

   This stores the current value of the file static STRUCT_PARSE_INFO
   in *ENCLOSING_STRUCT_PARSE_INFO, and points STRUCT_PARSE_INFO at a
   new c_struct_parse_info structure.  The old value of
   STRUCT_PARSE_INFO is restored in finish_struct.  */

tree
start_struct (location_t loc, enum tree_code code, tree name,
	      struct c_struct_parse_info **enclosing_struct_parse_info)
{
  /* If there is already a tag defined at this scope
     (as a forward reference), just return it.  */

  tree ref = NULL_TREE;
  location_t refloc = UNKNOWN_LOCATION;

  if (name != NULL_TREE)
    ref = lookup_tag (code, name, true, &refloc);
  if (ref && TREE_CODE (ref) == code)
    {
      if (TYPE_STUB_DECL (ref))
	refloc = DECL_SOURCE_LOCATION (TYPE_STUB_DECL (ref));

      if (TYPE_SIZE (ref))
	{
	  if (code == UNION_TYPE)
	    error_at (loc, "redefinition of %<union %E%>", name);
	  else
	    error_at (loc, "redefinition of %<struct %E%>", name);
	  if (refloc != UNKNOWN_LOCATION)
	    inform (refloc, "originally defined here");
	  /* Don't create structures using a name already in use.  */
	  ref = NULL_TREE;
	}
      else if (C_TYPE_BEING_DEFINED (ref))
	{
	  if (code == UNION_TYPE)
	    error_at (loc, "nested redefinition of %<union %E%>", name);
	  else
	    error_at (loc, "nested redefinition of %<struct %E%>", name);
	  /* Don't bother to report "originally defined here" for a
	     nested redefinition; the original definition should be
	     obvious.  */
	  /* Don't create structures that contain themselves.  */
	  ref = NULL_TREE;
	}
    }

  /* Otherwise create a forward-reference just so the tag is in scope.  */

  if (ref == NULL_TREE || TREE_CODE (ref) != code)
    {
      ref = make_node (code);
      pushtag (loc, name, ref);
    }

  C_TYPE_BEING_DEFINED (ref) = 1;
  for (tree v = TYPE_MAIN_VARIANT (ref); v; v = TYPE_NEXT_VARIANT (v))
    TYPE_PACKED (v) = flag_pack_struct;

  *enclosing_struct_parse_info = struct_parse_info;
  struct_parse_info = new c_struct_parse_info ();

  /* FIXME: This will issue a warning for a use of a type defined
     within a statement expr used within sizeof, et. al.  This is not
     terribly serious as C++ doesn't permit statement exprs within
     sizeof anyhow.  */
  if (warn_cxx_compat && (in_sizeof || in_typeof || in_alignof))
    warning_at (loc, OPT_Wc___compat,
		"defining type in %qs expression is invalid in C++",
		(in_sizeof
		 ? "sizeof"
		 : (in_typeof ? "typeof" : "alignof")));

  return ref;
}

/* Process the specs, declarator and width (NULL if omitted)
   of a structure component, returning a FIELD_DECL node.
   WIDTH is non-NULL for bit-fields only, and is an INTEGER_CST node.
   DECL_ATTRS is as for grokdeclarator.

   LOC is the location of the structure component.

   This is done during the parsing of the struct declaration.
   The FIELD_DECL nodes are chained together and the lot of them
   are ultimately passed to `build_struct' to make the RECORD_TYPE node.  */

tree
grokfield (location_t loc,
	   struct c_declarator *declarator, struct c_declspecs *declspecs,
	   tree width, tree *decl_attrs)
{
  tree value;

  if (declarator->kind == cdk_id && declarator->u.id == NULL_TREE
      && width == NULL_TREE)
    {
      /* This is an unnamed decl.

	 If we have something of the form "union { list } ;" then this
	 is the anonymous union extension.  Similarly for struct.

	 If this is something of the form "struct foo;", then
	   If MS or Plan 9 extensions are enabled, this is handled as
	     an anonymous struct.
	   Otherwise this is a forward declaration of a structure tag.

	 If this is something of the form "foo;" and foo is a TYPE_DECL, then
	   If foo names a structure or union without a tag, then this
	     is an anonymous struct (this is permitted by C11).
	   If MS or Plan 9 extensions are enabled and foo names a
	     structure, then again this is an anonymous struct.
	   Otherwise this is an error.

	 Oh what a horrid tangled web we weave.  I wonder if MS consciously
	 took this from Plan 9 or if it was an accident of implementation
	 that took root before someone noticed the bug...  */

      tree type = declspecs->type;
      bool ok = false;

      if (RECORD_OR_UNION_TYPE_P (type)
	  && (flag_ms_extensions
	      || flag_plan9_extensions
	      || !declspecs->typedef_p))
	{
	  if (flag_ms_extensions || flag_plan9_extensions)
	    ok = true;
	  else if (TYPE_NAME (type) == NULL)
	    ok = true;
	  else
	    ok = false;
	}
      if (!ok)
	{
	  pedwarn (loc, 0, "declaration does not declare anything");
	  return NULL_TREE;
	}
      if (flag_isoc99)
	pedwarn_c99 (loc, OPT_Wpedantic,
		     "ISO C99 doesn%'t support unnamed structs/unions");
      else
	pedwarn_c99 (loc, OPT_Wpedantic,
		     "ISO C90 doesn%'t support unnamed structs/unions");
    }

  value = grokdeclarator (declarator, declspecs, FIELD, false,
			  width ? &width : NULL, decl_attrs, NULL, NULL,
			  DEPRECATED_NORMAL);

  finish_decl (value, loc, NULL_TREE, NULL_TREE, NULL_TREE);
  DECL_INITIAL (value) = width;
  if (width)
    SET_DECL_C_BIT_FIELD (value);

  if (warn_cxx_compat && DECL_NAME (value) != NULL_TREE)
    {
      /* If we currently have a binding for this field, set the
	 in_struct field in the binding, so that we warn about lookups
	 which find it.  */
      struct c_binding *b = I_SYMBOL_BINDING (DECL_NAME (value));
      if (b != NULL)
	{
	  /* If the in_struct field is not yet set, push it on a list
	     to be cleared when this struct is finished.  */
	  if (!b->in_struct)
	    {
	      struct_parse_info->fields.safe_push (b);
	      b->in_struct = 1;
	    }
	}
    }

  return value;
}

/* Subroutine of detect_field_duplicates: return whether X and Y,
   which are both fields in the same struct, have duplicate field
   names.  */

static bool
is_duplicate_field (tree x, tree y)
{
  if (DECL_NAME (x) != NULL_TREE && DECL_NAME (x) == DECL_NAME (y))
    return true;

  /* When using -fplan9-extensions, an anonymous field whose name is a
     typedef can duplicate a field name.  */
  if (flag_plan9_extensions
      && (DECL_NAME (x) == NULL_TREE || DECL_NAME (y) == NULL_TREE))
    {
      tree xt, xn, yt, yn;

      xt = TREE_TYPE (x);
      if (DECL_NAME (x) != NULL_TREE)
	xn = DECL_NAME (x);
      else if (RECORD_OR_UNION_TYPE_P (xt)
	       && TYPE_NAME (xt) != NULL_TREE
	       && TREE_CODE (TYPE_NAME (xt)) == TYPE_DECL)
	xn = DECL_NAME (TYPE_NAME (xt));
      else
	xn = NULL_TREE;

      yt = TREE_TYPE (y);
      if (DECL_NAME (y) != NULL_TREE)
	yn = DECL_NAME (y);
      else if (RECORD_OR_UNION_TYPE_P (yt)
	       && TYPE_NAME (yt) != NULL_TREE
	       && TREE_CODE (TYPE_NAME (yt)) == TYPE_DECL)
	yn = DECL_NAME (TYPE_NAME (yt));
      else
	yn = NULL_TREE;

      if (xn != NULL_TREE && xn == yn)
	return true;
    }

  return false;
}

/* Subroutine of detect_field_duplicates: add the fields of FIELDLIST
   to HTAB, giving errors for any duplicates.  */

static void
detect_field_duplicates_hash (tree fieldlist,
			      hash_table<nofree_ptr_hash <tree_node> > *htab)
{
  tree x, y;
  tree_node **slot;

  for (x = fieldlist; x ; x = DECL_CHAIN (x))
    if ((y = DECL_NAME (x)) != NULL_TREE)
      {
	slot = htab->find_slot (y, INSERT);
	if (*slot)
	  {
	    error ("duplicate member %q+D", x);
	    DECL_NAME (x) = NULL_TREE;
	  }
	*slot = y;
      }
    else if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (x)))
      {
	detect_field_duplicates_hash (TYPE_FIELDS (TREE_TYPE (x)), htab);

	/* When using -fplan9-extensions, an anonymous field whose
	   name is a typedef can duplicate a field name.  */
	if (flag_plan9_extensions
	    && TYPE_NAME (TREE_TYPE (x)) != NULL_TREE
	    && TREE_CODE (TYPE_NAME (TREE_TYPE (x))) == TYPE_DECL)
	  {
	    tree xn = DECL_NAME (TYPE_NAME (TREE_TYPE (x)));
	    slot = htab->find_slot (xn, INSERT);
	    if (*slot)
	      error ("duplicate member %q+D", TYPE_NAME (TREE_TYPE (x)));
	    *slot = xn;
	  }
      }
}

/* Generate an error for any duplicate field names in FIELDLIST.  Munge
   the list such that this does not present a problem later.  */

static void
detect_field_duplicates (tree fieldlist)
{
  tree x, y;
  int timeout = 10;

  /* If the struct is the list of instance variables of an Objective-C
     class, then we need to check all the instance variables of
     superclasses when checking for duplicates (since you can't have
     an instance variable in a subclass with the same name as an
     instance variable in a superclass).  We pass on this job to the
     Objective-C compiler.  objc_detect_field_duplicates() will return
     false if we are not checking the list of instance variables and
     the C frontend should proceed with the standard field duplicate
     checks.  If we are checking the list of instance variables, the
     ObjC frontend will do the check, emit the errors if needed, and
     then return true.  */
  if (c_dialect_objc ())
    if (objc_detect_field_duplicates (false))
      return;

  /* First, see if there are more than "a few" fields.
     This is trivially true if there are zero or one fields.  */
  if (!fieldlist || !DECL_CHAIN (fieldlist))
    return;
  x = fieldlist;
  do {
    timeout--;
    if (DECL_NAME (x) == NULL_TREE
	&& RECORD_OR_UNION_TYPE_P (TREE_TYPE (x)))
      timeout = 0;
    x = DECL_CHAIN (x);
  } while (timeout > 0 && x);

  /* If there were "few" fields and no anonymous structures or unions,
     avoid the overhead of allocating a hash table.  Instead just do
     the nested traversal thing.  */
  if (timeout > 0)
    {
      for (x = DECL_CHAIN (fieldlist); x; x = DECL_CHAIN (x))
	/* When using -fplan9-extensions, we can have duplicates
	   between typedef names and fields.  */
	if (DECL_NAME (x)
	    || (flag_plan9_extensions
		&& DECL_NAME (x) == NULL_TREE
		&& RECORD_OR_UNION_TYPE_P (TREE_TYPE (x))
		&& TYPE_NAME (TREE_TYPE (x)) != NULL_TREE
		&& TREE_CODE (TYPE_NAME (TREE_TYPE (x))) == TYPE_DECL))
	  {
	    for (y = fieldlist; y != x; y = TREE_CHAIN (y))
	      if (is_duplicate_field (y, x))
		{
		  error ("duplicate member %q+D", x);
		  DECL_NAME (x) = NULL_TREE;
		}
	  }
    }
  else
    {
      hash_table<nofree_ptr_hash <tree_node> > htab (37);
      detect_field_duplicates_hash (fieldlist, &htab);
    }
}

/* Finish up struct info used by -Wc++-compat.  */

static void
warn_cxx_compat_finish_struct (tree fieldlist, enum tree_code code,
			       location_t record_loc)
{
  unsigned int ix;
  tree x;
  struct c_binding *b;

  if (fieldlist == NULL_TREE)
    {
      if (code == RECORD_TYPE)
	warning_at (record_loc, OPT_Wc___compat,
		    "empty struct has size 0 in C, size 1 in C++");
      else
	warning_at (record_loc, OPT_Wc___compat,
		    "empty union has size 0 in C, size 1 in C++");
    }

  /* Set the C_TYPE_DEFINED_IN_STRUCT flag for each type defined in
     the current struct.  We do this now at the end of the struct
     because the flag is used to issue visibility warnings, and we
     only want to issue those warnings if the type is referenced
     outside of the struct declaration.  */
  FOR_EACH_VEC_ELT (struct_parse_info->struct_types, ix, x)
    C_TYPE_DEFINED_IN_STRUCT (x) = 1;

  /* The TYPEDEFS_SEEN field of STRUCT_PARSE_INFO is a list of
     typedefs used when declaring fields in this struct.  If the name
     of any of the fields is also a typedef name then the struct would
     not parse in C++, because the C++ lookup rules say that the
     typedef name would be looked up in the context of the struct, and
     would thus be the field rather than the typedef.  */
  if (!struct_parse_info->typedefs_seen.is_empty ()
      && fieldlist != NULL_TREE)
    {
      /* Use a hash_set<tree> using the name of the typedef.  We can use
	 a hash_set<tree> because identifiers are interned.  */
      hash_set<tree> tset;

      FOR_EACH_VEC_ELT (struct_parse_info->typedefs_seen, ix, x)
	tset.add (DECL_NAME (x));

      for (x = fieldlist; x != NULL_TREE; x = DECL_CHAIN (x))
	{
	  if (DECL_NAME (x) != NULL_TREE
	      && tset.contains (DECL_NAME (x)))
	    {
	      warning_at (DECL_SOURCE_LOCATION (x), OPT_Wc___compat,
			  ("using %qD as both field and typedef name is "
			   "invalid in C++"),
			  x);
	      /* FIXME: It would be nice to report the location where
		 the typedef name is used.  */
	    }
	}
    }

  /* For each field which has a binding and which was not defined in
     an enclosing struct, clear the in_struct field.  */
  FOR_EACH_VEC_ELT (struct_parse_info->fields, ix, b)
    b->in_struct = 0;
}

/* Function to help qsort sort FIELD_DECLs by name order.  */

static int
field_decl_cmp (const void *x_p, const void *y_p)
{
  const tree *const x = (const tree *) x_p;
  const tree *const y = (const tree *) y_p;

  if (DECL_NAME (*x) == DECL_NAME (*y))
    /* A nontype is "greater" than a type.  */
    return (TREE_CODE (*y) == TYPE_DECL) - (TREE_CODE (*x) == TYPE_DECL);
  if (DECL_NAME (*x) == NULL_TREE)
    return -1;
  if (DECL_NAME (*y) == NULL_TREE)
    return 1;
  if (DECL_NAME (*x) < DECL_NAME (*y))
    return -1;
  return 1;
}

/* Fill in the fields of a RECORD_TYPE or UNION_TYPE node, T.
   LOC is the location of the RECORD_TYPE or UNION_TYPE's definition.
   FIELDLIST is a chain of FIELD_DECL nodes for the fields.
   ATTRIBUTES are attributes to be applied to the structure.

   ENCLOSING_STRUCT_PARSE_INFO is the value of STRUCT_PARSE_INFO when
   the struct was started.  */

tree
finish_struct (location_t loc, tree t, tree fieldlist, tree attributes,
	       struct c_struct_parse_info *enclosing_struct_parse_info)
{
  tree x;
  bool toplevel = file_scope == current_scope;

  /* If this type was previously laid out as a forward reference,
     make sure we lay it out again.  */

  TYPE_SIZE (t) = NULL_TREE;

  decl_attributes (&t, attributes, (int) ATTR_FLAG_TYPE_IN_PLACE);

  if (pedantic)
    {
      for (x = fieldlist; x; x = DECL_CHAIN (x))
	{
	  if (DECL_NAME (x) != NULL_TREE)
	    break;
	  if (flag_isoc11 && RECORD_OR_UNION_TYPE_P (TREE_TYPE (x)))
	    break;
	}

      if (x == NULL_TREE)
	{
	  if (TREE_CODE (t) == UNION_TYPE)
	    {
	      if (fieldlist)
		pedwarn (loc, OPT_Wpedantic, "union has no named members");
	      else
		pedwarn (loc, OPT_Wpedantic, "union has no members");
	    }
	  else
	    {
	      if (fieldlist)
		pedwarn (loc, OPT_Wpedantic, "struct has no named members");
	      else
		pedwarn (loc, OPT_Wpedantic, "struct has no members");
	    }
	}
    }

  /* Install struct as DECL_CONTEXT of each field decl.
     Also process specified field sizes, found in the DECL_INITIAL,
     storing 0 there after the type has been changed to precision equal
     to its width, rather than the precision of the specified standard
     type.  (Correct layout requires the original type to have been preserved
     until now.)  */

  bool saw_named_field = false;
  for (x = fieldlist; x; x = DECL_CHAIN (x))
    {
      if (TREE_TYPE (x) == error_mark_node)
	continue;

      DECL_CONTEXT (x) = t;

      /* If any field is const, the structure type is pseudo-const.  */
      if (TREE_READONLY (x))
	C_TYPE_FIELDS_READONLY (t) = 1;
      else
	{
	  /* A field that is pseudo-const makes the structure likewise.  */
	  tree t1 = strip_array_types (TREE_TYPE (x));
	  if (RECORD_OR_UNION_TYPE_P (t1) && C_TYPE_FIELDS_READONLY (t1))
	    C_TYPE_FIELDS_READONLY (t) = 1;
	}

      /* Any field that is volatile means variables of this type must be
	 treated in some ways as volatile.  */
      if (TREE_THIS_VOLATILE (x))
	C_TYPE_FIELDS_VOLATILE (t) = 1;

      /* Any field of nominal variable size implies structure is too.  */
      if (C_DECL_VARIABLE_SIZE (x))
	C_TYPE_VARIABLE_SIZE (t) = 1;

      if (DECL_C_BIT_FIELD (x))
	{
	  unsigned HOST_WIDE_INT width = tree_to_uhwi (DECL_INITIAL (x));
	  DECL_SIZE (x) = bitsize_int (width);
	  DECL_BIT_FIELD (x) = 1;
	}

      if (TYPE_PACKED (t)
	  && (DECL_BIT_FIELD (x)
	      || TYPE_ALIGN (TREE_TYPE (x)) > BITS_PER_UNIT))
	DECL_PACKED (x) = 1;

      /* Detect flexible array member in an invalid context.  */
      if (TREE_CODE (TREE_TYPE (x)) == ARRAY_TYPE
	  && TYPE_SIZE (TREE_TYPE (x)) == NULL_TREE
	  && TYPE_DOMAIN (TREE_TYPE (x)) != NULL_TREE
	  && TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (x))) == NULL_TREE)
	{
	  if (TREE_CODE (t) == UNION_TYPE)
	    {
	      error_at (DECL_SOURCE_LOCATION (x),
			"flexible array member in union");
	      TREE_TYPE (x) = error_mark_node;
	    }
	  else if (DECL_CHAIN (x) != NULL_TREE)
	    {
	      error_at (DECL_SOURCE_LOCATION (x),
			"flexible array member not at end of struct");
	      TREE_TYPE (x) = error_mark_node;
	    }
	  else if (!saw_named_field)
	    {
	      error_at (DECL_SOURCE_LOCATION (x),
			"flexible array member in a struct with no named "
			"members");
	      TREE_TYPE (x) = error_mark_node;
	    }
	}

      if (pedantic && TREE_CODE (t) == RECORD_TYPE
	  && flexible_array_type_p (TREE_TYPE (x)))
	pedwarn (DECL_SOURCE_LOCATION (x), OPT_Wpedantic,
		 "invalid use of structure with flexible array member");

      if (DECL_NAME (x)
	  || RECORD_OR_UNION_TYPE_P (TREE_TYPE (x)))
	saw_named_field = true;
    }

  detect_field_duplicates (fieldlist);

  /* Now we have the nearly final fieldlist.  Record it,
     then lay out the structure or union (including the fields).  */

  TYPE_FIELDS (t) = fieldlist;

  maybe_apply_pragma_scalar_storage_order (t);

  layout_type (t);

  if (TYPE_SIZE_UNIT (t)
      && TREE_CODE (TYPE_SIZE_UNIT (t)) == INTEGER_CST
      && !TREE_OVERFLOW (TYPE_SIZE_UNIT (t))
      && !valid_constant_size_p (TYPE_SIZE_UNIT (t)))
    error ("type %qT is too large", t);

  /* Give bit-fields their proper types and rewrite the type of array fields
     with scalar component if the enclosing type has reverse storage order.  */
  for (tree field = fieldlist; field; field = DECL_CHAIN (field))
    {
      if (TREE_CODE (field) == FIELD_DECL
	  && DECL_INITIAL (field)
	  && TREE_TYPE (field) != error_mark_node)
	{
	  unsigned HOST_WIDE_INT width
	    = tree_to_uhwi (DECL_INITIAL (field));
	  tree type = TREE_TYPE (field);
	  if (width != TYPE_PRECISION (type))
	    {
	      TREE_TYPE (field)
		= c_build_bitfield_integer_type (width, TYPE_UNSIGNED (type));
	      SET_DECL_MODE (field, TYPE_MODE (TREE_TYPE (field)));
	    }
	  DECL_INITIAL (field) = NULL_TREE;
	}
      else if (TYPE_REVERSE_STORAGE_ORDER (t)
	       && TREE_CODE (field) == FIELD_DECL
	       && TREE_CODE (TREE_TYPE (field)) == ARRAY_TYPE)
	{
	  tree ftype = TREE_TYPE (field);
	  tree ctype = strip_array_types (ftype);
	  if (!RECORD_OR_UNION_TYPE_P (ctype) && TYPE_MODE (ctype) != QImode)
	    {
	      tree fmain_type = TYPE_MAIN_VARIANT (ftype);
	      tree *typep = &fmain_type;
	      do {
		*typep = build_distinct_type_copy (*typep);
		TYPE_REVERSE_STORAGE_ORDER (*typep) = 1;
		typep = &TREE_TYPE (*typep);
	      } while (TREE_CODE (*typep) == ARRAY_TYPE);
	      TREE_TYPE (field)
		= c_build_qualified_type (fmain_type, TYPE_QUALS (ftype));
	    }
	}
    }

  /* Now we have the truly final field list.
     Store it in this type and in the variants.  */

  TYPE_FIELDS (t) = fieldlist;

  /* If there are lots of fields, sort so we can look through them fast.
     We arbitrarily consider 16 or more elts to be "a lot".  */

  {
    int len = 0;

    for (x = fieldlist; x; x = DECL_CHAIN (x))
      {
	if (len > 15 || DECL_NAME (x) == NULL)
	  break;
	len += 1;
      }

    if (len > 15)
      {
	tree *field_array;
	struct lang_type *space;
	struct sorted_fields_type *space2;

	len += list_length (x);

	/* Use the same allocation policy here that make_node uses, to
	  ensure that this lives as long as the rest of the struct decl.
	  All decls in an inline function need to be saved.  */

	space = ggc_cleared_alloc<struct lang_type> ();
	space2 = (sorted_fields_type *) ggc_internal_alloc
	  (sizeof (struct sorted_fields_type) + len * sizeof (tree));

	len = 0;
	space->s = space2;
	field_array = &space2->elts[0];
	for (x = fieldlist; x; x = DECL_CHAIN (x))
	  {
	    field_array[len++] = x;

	    /* If there is anonymous struct or union, break out of the loop.  */
	    if (DECL_NAME (x) == NULL)
	      break;
	  }
	/* Found no anonymous struct/union.  Add the TYPE_LANG_SPECIFIC.  */
	if (x == NULL)
	  {
	    TYPE_LANG_SPECIFIC (t) = space;
	    TYPE_LANG_SPECIFIC (t)->s->len = len;
	    field_array = TYPE_LANG_SPECIFIC (t)->s->elts;
	    qsort (field_array, len, sizeof (tree), field_decl_cmp);
	  }
      }
  }

  /* Note: C_TYPE_INCOMPLETE_VARS overloads TYPE_VFIELD which is used
     in dwarf2out via rest_of_decl_compilation below and means
     something totally different.  Since we will be clearing
     C_TYPE_INCOMPLETE_VARS shortly after we iterate through them,
     clear it ahead of time and avoid problems in dwarf2out.  Ideally,
     C_TYPE_INCOMPLETE_VARS should use some language specific
     node.  */
  tree incomplete_vars = C_TYPE_INCOMPLETE_VARS (TYPE_MAIN_VARIANT (t));
  for (x = TYPE_MAIN_VARIANT (t); x; x = TYPE_NEXT_VARIANT (x))
    {
      TYPE_FIELDS (x) = TYPE_FIELDS (t);
      TYPE_LANG_SPECIFIC (x) = TYPE_LANG_SPECIFIC (t);
      C_TYPE_FIELDS_READONLY (x) = C_TYPE_FIELDS_READONLY (t);
      C_TYPE_FIELDS_VOLATILE (x) = C_TYPE_FIELDS_VOLATILE (t);
      C_TYPE_VARIABLE_SIZE (x) = C_TYPE_VARIABLE_SIZE (t);
      C_TYPE_INCOMPLETE_VARS (x) = NULL_TREE;
    }

  /* If this was supposed to be a transparent union, but we can't
     make it one, warn and turn off the flag.  */
  if (TREE_CODE (t) == UNION_TYPE
      && TYPE_TRANSPARENT_AGGR (t)
      && (!TYPE_FIELDS (t) || TYPE_MODE (t) != DECL_MODE (TYPE_FIELDS (t))))
    {
      TYPE_TRANSPARENT_AGGR (t) = 0;
      warning_at (loc, 0, "union cannot be made transparent");
    }

  /* If this structure or union completes the type of any previous
     variable declaration, lay it out and output its rtl.  */
  for (x = incomplete_vars; x; x = TREE_CHAIN (x))
    {
      tree decl = TREE_VALUE (x);
      if (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	layout_array_type (TREE_TYPE (decl));
      if (TREE_CODE (decl) != TYPE_DECL)
	{
	  layout_decl (decl, 0);
	  if (c_dialect_objc ())
	    objc_check_decl (decl);
	  rest_of_decl_compilation (decl, toplevel, 0);
	}
    }

  /* Update type location to the one of the definition, instead of e.g.
     a forward declaration.  */
  if (TYPE_STUB_DECL (t))
    DECL_SOURCE_LOCATION (TYPE_STUB_DECL (t)) = loc;

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (t, toplevel);

  /* If we're inside a function proper, i.e. not file-scope and not still
     parsing parameters, then arrange for the size of a variable sized type
     to be bound now.  */
  if (building_stmt_list_p () && variably_modified_type_p (t, NULL_TREE))
    add_stmt (build_stmt (loc,
			  DECL_EXPR, build_decl (loc, TYPE_DECL, NULL, t)));

  if (warn_cxx_compat)
    warn_cxx_compat_finish_struct (fieldlist, TREE_CODE (t), loc);

  delete struct_parse_info;

  struct_parse_info = enclosing_struct_parse_info;

  /* If this struct is defined inside a struct, add it to
     struct_types.  */
  if (warn_cxx_compat
      && struct_parse_info != NULL
      && !in_sizeof && !in_typeof && !in_alignof)
    struct_parse_info->struct_types.safe_push (t);

  return t;
}

static struct {
  gt_pointer_operator new_value;
  void *cookie;
} resort_data;

/* This routine compares two fields like field_decl_cmp but using the
pointer operator in resort_data.  */

static int
resort_field_decl_cmp (const void *x_p, const void *y_p)
{
  const tree *const x = (const tree *) x_p;
  const tree *const y = (const tree *) y_p;

  if (DECL_NAME (*x) == DECL_NAME (*y))
    /* A nontype is "greater" than a type.  */
    return (TREE_CODE (*y) == TYPE_DECL) - (TREE_CODE (*x) == TYPE_DECL);
  if (DECL_NAME (*x) == NULL_TREE)
    return -1;
  if (DECL_NAME (*y) == NULL_TREE)
    return 1;
  {
    tree d1 = DECL_NAME (*x);
    tree d2 = DECL_NAME (*y);
    resort_data.new_value (&d1, resort_data.cookie);
    resort_data.new_value (&d2, resort_data.cookie);
    if (d1 < d2)
      return -1;
  }
  return 1;
}

/* Resort DECL_SORTED_FIELDS because pointers have been reordered.  */

void
resort_sorted_fields (void *obj,
		      void * ARG_UNUSED (orig_obj),
		      gt_pointer_operator new_value,
		      void *cookie)
{
  struct sorted_fields_type *sf = (struct sorted_fields_type *) obj;
  resort_data.new_value = new_value;
  resort_data.cookie = cookie;
  qsort (&sf->elts[0], sf->len, sizeof (tree),
	 resort_field_decl_cmp);
}

/* Lay out the type T, and its element type, and so on.  */

static void
layout_array_type (tree t)
{
  if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
    layout_array_type (TREE_TYPE (t));
  layout_type (t);
}

/* Begin compiling the definition of an enumeration type.
   NAME is its name (or null if anonymous).
   LOC is the enum's location.
   Returns the type object, as yet incomplete.
   Also records info about it so that build_enumerator
   may be used to declare the individual values as they are read.  */

tree
start_enum (location_t loc, struct c_enum_contents *the_enum, tree name)
{
  tree enumtype = NULL_TREE;
  location_t enumloc = UNKNOWN_LOCATION;

  /* If this is the real definition for a previous forward reference,
     fill in the contents in the same object that used to be the
     forward reference.  */

  if (name != NULL_TREE)
    enumtype = lookup_tag (ENUMERAL_TYPE, name, true, &enumloc);

  if (enumtype == NULL_TREE || TREE_CODE (enumtype) != ENUMERAL_TYPE)
    {
      enumtype = make_node (ENUMERAL_TYPE);
      pushtag (loc, name, enumtype);
    }
  /* Update type location to the one of the definition, instead of e.g.
     a forward declaration.  */
  else if (TYPE_STUB_DECL (enumtype))
    {
      enumloc = DECL_SOURCE_LOCATION (TYPE_STUB_DECL (enumtype));
      DECL_SOURCE_LOCATION (TYPE_STUB_DECL (enumtype)) = loc;
    }

  if (C_TYPE_BEING_DEFINED (enumtype))
    error_at (loc, "nested redefinition of %<enum %E%>", name);

  C_TYPE_BEING_DEFINED (enumtype) = 1;

  if (TYPE_VALUES (enumtype) != NULL_TREE)
    {
      /* This enum is a named one that has been declared already.  */
      error_at (loc, "redeclaration of %<enum %E%>", name);
      if (enumloc != UNKNOWN_LOCATION)
	inform (enumloc, "originally defined here");

      /* Completely replace its old definition.
	 The old enumerators remain defined, however.  */
      TYPE_VALUES (enumtype) = NULL_TREE;
    }

  the_enum->enum_next_value = integer_zero_node;
  the_enum->enum_overflow = 0;

  if (flag_short_enums)
    for (tree v = TYPE_MAIN_VARIANT (enumtype); v; v = TYPE_NEXT_VARIANT (v))
      TYPE_PACKED (v) = 1;

  /* FIXME: This will issue a warning for a use of a type defined
     within sizeof in a statement expr.  This is not terribly serious
     as C++ doesn't permit statement exprs within sizeof anyhow.  */
  if (warn_cxx_compat && (in_sizeof || in_typeof || in_alignof))
    warning_at (loc, OPT_Wc___compat,
		"defining type in %qs expression is invalid in C++",
		(in_sizeof
		 ? "sizeof"
		 : (in_typeof ? "typeof" : "alignof")));

  return enumtype;
}

/* After processing and defining all the values of an enumeration type,
   install their decls in the enumeration type and finish it off.
   ENUMTYPE is the type object, VALUES a list of decl-value pairs,
   and ATTRIBUTES are the specified attributes.
   Returns ENUMTYPE.  */

tree
finish_enum (tree enumtype, tree values, tree attributes)
{
  tree pair, tem;
  tree minnode = NULL_TREE, maxnode = NULL_TREE;
  int precision;
  signop sign;
  bool toplevel = (file_scope == current_scope);
  struct lang_type *lt;

  decl_attributes (&enumtype, attributes, (int) ATTR_FLAG_TYPE_IN_PLACE);

  /* Calculate the maximum value of any enumerator in this type.  */

  if (values == error_mark_node)
    minnode = maxnode = integer_zero_node;
  else
    {
      minnode = maxnode = TREE_VALUE (values);
      for (pair = TREE_CHAIN (values); pair; pair = TREE_CHAIN (pair))
	{
	  tree value = TREE_VALUE (pair);
	  if (tree_int_cst_lt (maxnode, value))
	    maxnode = value;
	  if (tree_int_cst_lt (value, minnode))
	    minnode = value;
	}
    }

  /* Construct the final type of this enumeration.  It is the same
     as one of the integral types - the narrowest one that fits, except
     that normally we only go as narrow as int - and signed iff any of
     the values are negative.  */
  sign = (tree_int_cst_sgn (minnode) >= 0) ? UNSIGNED : SIGNED;
  precision = MAX (tree_int_cst_min_precision (minnode, sign),
		   tree_int_cst_min_precision (maxnode, sign));

  /* If the precision of the type was specified with an attribute and it
     was too small, give an error.  Otherwise, use it.  */
  if (TYPE_PRECISION (enumtype) && lookup_attribute ("mode", attributes))
    {
      if (precision > TYPE_PRECISION (enumtype))
	{
	  TYPE_PRECISION (enumtype) = 0;
	  error ("specified mode too small for enumeral values");
	}
      else
	precision = TYPE_PRECISION (enumtype);
    }
  else
    TYPE_PRECISION (enumtype) = 0;

  if (TYPE_PACKED (enumtype)
      || precision > TYPE_PRECISION (integer_type_node)
      || TYPE_PRECISION (enumtype))
    {
      tem = c_common_type_for_size (precision, sign == UNSIGNED ? 1 : 0);
      if (tem == NULL)
	{
	  warning (0, "enumeration values exceed range of largest integer");
	  tem = long_long_integer_type_node;
	}
    }
  else
    tem = sign == UNSIGNED ? unsigned_type_node : integer_type_node;

  TYPE_MIN_VALUE (enumtype) = TYPE_MIN_VALUE (tem);
  TYPE_MAX_VALUE (enumtype) = TYPE_MAX_VALUE (tem);
  TYPE_UNSIGNED (enumtype) = TYPE_UNSIGNED (tem);
  SET_TYPE_ALIGN (enumtype, TYPE_ALIGN (tem));
  TYPE_SIZE (enumtype) = NULL_TREE;
  TYPE_PRECISION (enumtype) = TYPE_PRECISION (tem);

  layout_type (enumtype);

  if (values != error_mark_node)
    {
      /* Change the type of the enumerators to be the enum type.  We
	 need to do this irrespective of the size of the enum, for
	 proper type checking.  Replace the DECL_INITIALs of the
	 enumerators, and the value slots of the list, with copies
	 that have the enum type; they cannot be modified in place
	 because they may be shared (e.g.  integer_zero_node) Finally,
	 change the purpose slots to point to the names of the decls.  */
      for (pair = values; pair; pair = TREE_CHAIN (pair))
	{
	  tree enu = TREE_PURPOSE (pair);
	  tree ini = DECL_INITIAL (enu);

	  TREE_TYPE (enu) = enumtype;

	  /* The ISO C Standard mandates enumerators to have type int,
	     even though the underlying type of an enum type is
	     unspecified.  However, GCC allows enumerators of any
	     integer type as an extensions.  build_enumerator()
	     converts any enumerators that fit in an int to type int,
	     to avoid promotions to unsigned types when comparing
	     integers with enumerators that fit in the int range.
	     When -pedantic is given, build_enumerator() would have
	     already warned about those that don't fit. Here we
	     convert the rest to the enumerator type. */
	  if (TREE_TYPE (ini) != integer_type_node)
	    ini = convert (enumtype, ini);

	  DECL_INITIAL (enu) = ini;
	  TREE_PURPOSE (pair) = DECL_NAME (enu);
	  TREE_VALUE (pair) = ini;
	}

      TYPE_VALUES (enumtype) = values;
    }

  /* Record the min/max values so that we can warn about bit-field
     enumerations that are too small for the values.  */
  lt = ggc_cleared_alloc<struct lang_type> ();
  lt->enum_min = minnode;
  lt->enum_max = maxnode;
  TYPE_LANG_SPECIFIC (enumtype) = lt;

  /* Fix up all variant types of this enum type.  */
  for (tem = TYPE_MAIN_VARIANT (enumtype); tem; tem = TYPE_NEXT_VARIANT (tem))
    {
      if (tem == enumtype)
	continue;
      TYPE_VALUES (tem) = TYPE_VALUES (enumtype);
      TYPE_MIN_VALUE (tem) = TYPE_MIN_VALUE (enumtype);
      TYPE_MAX_VALUE (tem) = TYPE_MAX_VALUE (enumtype);
      TYPE_SIZE (tem) = TYPE_SIZE (enumtype);
      TYPE_SIZE_UNIT (tem) = TYPE_SIZE_UNIT (enumtype);
      SET_TYPE_MODE (tem, TYPE_MODE (enumtype));
      TYPE_PRECISION (tem) = TYPE_PRECISION (enumtype);
      SET_TYPE_ALIGN (tem, TYPE_ALIGN (enumtype));
      TYPE_USER_ALIGN (tem) = TYPE_USER_ALIGN (enumtype);
      TYPE_UNSIGNED (tem) = TYPE_UNSIGNED (enumtype);
      TYPE_LANG_SPECIFIC (tem) = TYPE_LANG_SPECIFIC (enumtype);
    }

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (enumtype, toplevel);

  /* If this enum is defined inside a struct, add it to
     struct_types.  */
  if (warn_cxx_compat
      && struct_parse_info != NULL
      && !in_sizeof && !in_typeof && !in_alignof)
    struct_parse_info->struct_types.safe_push (enumtype);

  return enumtype;
}

/* Build and install a CONST_DECL for one value of the
   current enumeration type (one that was begun with start_enum).
   DECL_LOC is the location of the enumerator.
   LOC is the location of the '=' operator if any, DECL_LOC otherwise.
   Return a tree-list containing the CONST_DECL and its value.
   Assignment of sequential values by default is handled here.  */

tree
build_enumerator (location_t decl_loc, location_t loc,
		  struct c_enum_contents *the_enum, tree name, tree value)
{
  tree decl, type;

  /* Validate and default VALUE.  */

  if (value != NULL_TREE)
    {
      /* Don't issue more errors for error_mark_node (i.e. an
	 undeclared identifier) - just ignore the value expression.  */
      if (value == error_mark_node)
	value = NULL_TREE;
      else if (!INTEGRAL_TYPE_P (TREE_TYPE (value)))
	{
	  error_at (loc, "enumerator value for %qE is not an integer constant",
		    name);
	  value = NULL_TREE;
	}
      else
	{
	  if (TREE_CODE (value) != INTEGER_CST)
	    {
	      value = c_fully_fold (value, false, NULL);
	      if (TREE_CODE (value) == INTEGER_CST)
		pedwarn (loc, OPT_Wpedantic,
			 "enumerator value for %qE is not an integer "
			 "constant expression", name);
	    }
	  if (TREE_CODE (value) != INTEGER_CST)
	    {
	      error ("enumerator value for %qE is not an integer constant",
		     name);
	      value = NULL_TREE;
	    }
	  else
	    {
	      value = default_conversion (value);
	      constant_expression_warning (value);
	    }
	}
    }

  /* Default based on previous value.  */
  /* It should no longer be possible to have NON_LVALUE_EXPR
     in the default.  */
  if (value == NULL_TREE)
    {
      value = the_enum->enum_next_value;
      if (the_enum->enum_overflow)
	error_at (loc, "overflow in enumeration values");
    }
  /* Even though the underlying type of an enum is unspecified, the
     type of enumeration constants is explicitly defined as int
     (6.4.4.3/2 in the C99 Standard).  GCC allows any integer type as
     an extension.  */
  else if (!int_fits_type_p (value, integer_type_node))
    pedwarn (loc, OPT_Wpedantic,
	     "ISO C restricts enumerator values to range of %<int%>");

  /* The ISO C Standard mandates enumerators to have type int, even
     though the underlying type of an enum type is unspecified.
     However, GCC allows enumerators of any integer type as an
     extensions.  Here we convert any enumerators that fit in an int
     to type int, to avoid promotions to unsigned types when comparing
     integers with enumerators that fit in the int range.  When
     -pedantic is given, we would have already warned about those that
     don't fit. We have to do this here rather than in finish_enum
     because this value may be used to define more enumerators.  */
  if (int_fits_type_p (value, integer_type_node))
    value = convert (integer_type_node, value);

  /* Set basis for default for next value.  */
  the_enum->enum_next_value
    = build_binary_op (EXPR_LOC_OR_LOC (value, input_location),
		       PLUS_EXPR, value, integer_one_node, false);
  the_enum->enum_overflow = tree_int_cst_lt (the_enum->enum_next_value, value);

  /* Now create a declaration for the enum value name.  */

  type = TREE_TYPE (value);
  type = c_common_type_for_size (MAX (TYPE_PRECISION (type),
				      TYPE_PRECISION (integer_type_node)),
				 (TYPE_PRECISION (type)
				  >= TYPE_PRECISION (integer_type_node)
				  && TYPE_UNSIGNED (type)));

  decl = build_decl (decl_loc, CONST_DECL, name, type);
  DECL_INITIAL (decl) = convert (type, value);
  pushdecl (decl);

  return tree_cons (decl, value, NULL_TREE);
}


/* Create the FUNCTION_DECL for a function definition.
   DECLSPECS, DECLARATOR and ATTRIBUTES are the parts of
   the declaration; they describe the function's name and the type it returns,
   but twisted together in a fashion that parallels the syntax of C.

   This function creates a binding context for the function body
   as well as setting up the FUNCTION_DECL in current_function_decl.

   Returns true on success.  If the DECLARATOR is not suitable for a function
   (it defines a datum instead), we return false to report a parse error.  */

bool
start_function (struct c_declspecs *declspecs, struct c_declarator *declarator,
		tree attributes)
{
  tree decl1, old_decl;
  tree restype, resdecl;
  location_t loc;

  current_function_returns_value = 0;  /* Assume, until we see it does.  */
  current_function_returns_null = 0;
  current_function_returns_abnormally = 0;
  warn_about_return_type = 0;
  c_switch_stack = NULL;

  /* Indicate no valid break/continue context by setting these variables
     to some non-null, non-label value.  We'll notice and emit the proper
     error message in c_finish_bc_stmt.  */
  c_break_label = c_cont_label = size_zero_node;

  decl1 = grokdeclarator (declarator, declspecs, FUNCDEF, true, NULL,
			  &attributes, NULL, NULL, DEPRECATED_NORMAL);
  invoke_plugin_callbacks (PLUGIN_START_PARSE_FUNCTION, decl1);

  /* If the declarator is not suitable for a function definition,
     cause a syntax error.  */
  if (decl1 == NULL_TREE
      || TREE_CODE (decl1) != FUNCTION_DECL)
    return false;

  loc = DECL_SOURCE_LOCATION (decl1);

  c_decl_attributes (&decl1, attributes, 0);

  if (DECL_DECLARED_INLINE_P (decl1)
      && DECL_UNINLINABLE (decl1)
      && lookup_attribute ("noinline", DECL_ATTRIBUTES (decl1)))
    warning_at (loc, OPT_Wattributes,
		"inline function %qD given attribute noinline",
		decl1);

  /* Handle gnu_inline attribute.  */
  if (declspecs->inline_p
      && !flag_gnu89_inline
      && TREE_CODE (decl1) == FUNCTION_DECL
      && (lookup_attribute ("gnu_inline", DECL_ATTRIBUTES (decl1))
	  || current_function_decl))
    {
      if (declspecs->storage_class != csc_static)
	DECL_EXTERNAL (decl1) = !DECL_EXTERNAL (decl1);
    }

  announce_function (decl1);

  if (!COMPLETE_OR_VOID_TYPE_P (TREE_TYPE (TREE_TYPE (decl1))))
    {
      error_at (loc, "return type is an incomplete type");
      /* Make it return void instead.  */
      TREE_TYPE (decl1)
	= build_function_type (void_type_node,
			       TYPE_ARG_TYPES (TREE_TYPE (decl1)));
    }

  if (warn_about_return_type)
    warn_defaults_to (loc, flag_isoc99 ? OPT_Wimplicit_int
			   : (warn_return_type ? OPT_Wreturn_type
			      : OPT_Wimplicit_int),
		      "return type defaults to %<int%>");

  /* Make the init_value nonzero so pushdecl knows this is not tentative.
     error_mark_node is replaced below (in pop_scope) with the BLOCK.  */
  DECL_INITIAL (decl1) = error_mark_node;

  /* A nested function is not global.  */
  if (current_function_decl != NULL_TREE)
    TREE_PUBLIC (decl1) = 0;

  /* If this definition isn't a prototype and we had a prototype declaration
     before, copy the arg type info from that prototype.  */
  old_decl = lookup_name_in_scope (DECL_NAME (decl1), current_scope);
  if (old_decl && TREE_CODE (old_decl) != FUNCTION_DECL)
    old_decl = NULL_TREE;
  current_function_prototype_locus = UNKNOWN_LOCATION;
  current_function_prototype_built_in = false;
  current_function_prototype_arg_types = NULL_TREE;
  if (!prototype_p (TREE_TYPE (decl1)))
    {
      if (old_decl != NULL_TREE
	  && TREE_CODE (TREE_TYPE (old_decl)) == FUNCTION_TYPE
	  && comptypes (TREE_TYPE (TREE_TYPE (decl1)),
			TREE_TYPE (TREE_TYPE (old_decl))))
	{
	  if (stdarg_p (TREE_TYPE (old_decl)))
	    {
	      warning_at (loc, 0, "%q+D defined as variadic function "
			  "without prototype", decl1);
	      locate_old_decl (old_decl);
	    }
	  TREE_TYPE (decl1) = composite_type (TREE_TYPE (old_decl),
					      TREE_TYPE (decl1));
	  current_function_prototype_locus = DECL_SOURCE_LOCATION (old_decl);
	  current_function_prototype_built_in
	    = C_DECL_BUILTIN_PROTOTYPE (old_decl);
	  current_function_prototype_arg_types
	    = TYPE_ARG_TYPES (TREE_TYPE (decl1));
	}
      if (TREE_PUBLIC (decl1))
	{
	  /* If there is an external prototype declaration of this
	     function, record its location but do not copy information
	     to this decl.  This may be an invisible declaration
	     (built-in or in a scope which has finished) or simply
	     have more refined argument types than any declaration
	     found above.  */
	  struct c_binding *b;
	  for (b = I_SYMBOL_BINDING (DECL_NAME (decl1)); b; b = b->shadowed)
	    if (B_IN_SCOPE (b, external_scope))
	      break;
	  if (b)
	    {
	      tree ext_decl, ext_type;
	      ext_decl = b->decl;
	      ext_type = b->u.type ? b->u.type : TREE_TYPE (ext_decl);
	      if (TREE_CODE (ext_type) == FUNCTION_TYPE
		  && comptypes (TREE_TYPE (TREE_TYPE (decl1)),
				TREE_TYPE (ext_type)))
		{
		  current_function_prototype_locus
		    = DECL_SOURCE_LOCATION (ext_decl);
		  current_function_prototype_built_in
		    = C_DECL_BUILTIN_PROTOTYPE (ext_decl);
		  current_function_prototype_arg_types
		    = TYPE_ARG_TYPES (ext_type);
		}
	    }
	}
    }

  /* Optionally warn of old-fashioned def with no previous prototype.  */
  if (warn_strict_prototypes
      && old_decl != error_mark_node
      && !prototype_p (TREE_TYPE (decl1))
      && C_DECL_ISNT_PROTOTYPE (old_decl))
    warning_at (loc, OPT_Wstrict_prototypes,
		"function declaration isn%'t a prototype");
  /* Optionally warn of any global def with no previous prototype.  */
  else if (warn_missing_prototypes
	   && old_decl != error_mark_node
	   && TREE_PUBLIC (decl1)
	   && !MAIN_NAME_P (DECL_NAME (decl1))
	   && C_DECL_ISNT_PROTOTYPE (old_decl)
	   && !DECL_DECLARED_INLINE_P (decl1))
    warning_at (loc, OPT_Wmissing_prototypes,
		"no previous prototype for %qD", decl1);
  /* Optionally warn of any def with no previous prototype
     if the function has already been used.  */
  else if (warn_missing_prototypes
	   && old_decl != NULL_TREE
	   && old_decl != error_mark_node
	   && TREE_USED (old_decl)
	   && !prototype_p (TREE_TYPE (old_decl)))
    warning_at (loc, OPT_Wmissing_prototypes,
		"%qD was used with no prototype before its definition", decl1);
  /* Optionally warn of any global def with no previous declaration.  */
  else if (warn_missing_declarations
	   && TREE_PUBLIC (decl1)
	   && old_decl == NULL_TREE
	   && !MAIN_NAME_P (DECL_NAME (decl1))
	   && !DECL_DECLARED_INLINE_P (decl1))
    warning_at (loc, OPT_Wmissing_declarations,
		"no previous declaration for %qD",
		decl1);
  /* Optionally warn of any def with no previous declaration
     if the function has already been used.  */
  else if (warn_missing_declarations
	   && old_decl != NULL_TREE
	   && old_decl != error_mark_node
	   && TREE_USED (old_decl)
	   && C_DECL_IMPLICIT (old_decl))
    warning_at (loc, OPT_Wmissing_declarations,
		"%qD was used with no declaration before its definition", decl1);

  /* This function exists in static storage.
     (This does not mean `static' in the C sense!)  */
  TREE_STATIC (decl1) = 1;

  /* This is the earliest point at which we might know the assembler
     name of the function.  Thus, if it's set before this, die horribly.  */
  gcc_assert (!DECL_ASSEMBLER_NAME_SET_P (decl1));

  /* If #pragma weak was used, mark the decl weak now.  */
  if (current_scope == file_scope)
    maybe_apply_pragma_weak (decl1);

  /* Warn for unlikely, improbable, or stupid declarations of `main'.  */
  if (warn_main && MAIN_NAME_P (DECL_NAME (decl1)))
    {
      if (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (decl1)))
	  != integer_type_node)
	pedwarn (loc, OPT_Wmain, "return type of %qD is not %<int%>", decl1);
      else if (TYPE_ATOMIC (TREE_TYPE (TREE_TYPE (decl1))))
	pedwarn (loc, OPT_Wmain, "%<_Atomic%>-qualified return type of %qD",
		 decl1);

      check_main_parameter_types (decl1);

      if (!TREE_PUBLIC (decl1))
	pedwarn (loc, OPT_Wmain,
		 "%qD is normally a non-static function", decl1);
    }

  /* Record the decl so that the function name is defined.
     If we already have a decl for this name, and it is a FUNCTION_DECL,
     use the old decl.  */

  current_function_decl = pushdecl (decl1);

  push_scope ();
  declare_parm_level ();

  restype = TREE_TYPE (TREE_TYPE (current_function_decl));
  resdecl = build_decl (loc, RESULT_DECL, NULL_TREE, restype);
  DECL_ARTIFICIAL (resdecl) = 1;
  DECL_IGNORED_P (resdecl) = 1;
  DECL_RESULT (current_function_decl) = resdecl;

  start_fname_decls ();

  return true;
}

/* Subroutine of store_parm_decls which handles new-style function
   definitions (prototype format). The parms already have decls, so we
   need only record them as in effect and complain if any redundant
   old-style parm decls were written.  */
static void
store_parm_decls_newstyle (tree fndecl, const struct c_arg_info *arg_info)
{
  tree decl;
  c_arg_tag *tag;
  unsigned ix;

  if (current_scope->bindings)
    {
      error_at (DECL_SOURCE_LOCATION (fndecl),
		"old-style parameter declarations in prototyped "
		"function definition");

      /* Get rid of the old-style declarations.  */
      pop_scope ();
      push_scope ();
    }
  /* Don't issue this warning for nested functions, and don't issue this
     warning if we got here because ARG_INFO_TYPES was error_mark_node
     (this happens when a function definition has just an ellipsis in
     its parameter list).  */
  else if (!in_system_header_at (input_location)
	   && !current_function_scope
	   && arg_info->types != error_mark_node)
    warning_at (DECL_SOURCE_LOCATION (fndecl), OPT_Wtraditional,
		"traditional C rejects ISO C style function definitions");

  /* Now make all the parameter declarations visible in the function body.
     We can bypass most of the grunt work of pushdecl.  */
  for (decl = arg_info->parms; decl; decl = DECL_CHAIN (decl))
    {
      DECL_CONTEXT (decl) = current_function_decl;
      if (DECL_NAME (decl))
	{
	  bind (DECL_NAME (decl), decl, current_scope,
		/*invisible=*/false, /*nested=*/false,
		UNKNOWN_LOCATION);
	  if (!TREE_USED (decl))
	    warn_if_shadowing (decl);
	}
      else
	error_at (DECL_SOURCE_LOCATION (decl), "parameter name omitted");
    }

  /* Record the parameter list in the function declaration.  */
  DECL_ARGUMENTS (fndecl) = arg_info->parms;

  /* Now make all the ancillary declarations visible, likewise.  */
  for (decl = arg_info->others; decl; decl = DECL_CHAIN (decl))
    {
      DECL_CONTEXT (decl) = current_function_decl;
      if (DECL_NAME (decl))
	bind (DECL_NAME (decl), decl, current_scope,
	      /*invisible=*/false,
	      /*nested=*/(TREE_CODE (decl) == FUNCTION_DECL),
	      UNKNOWN_LOCATION);
    }

  /* And all the tag declarations.  */
  FOR_EACH_VEC_SAFE_ELT_REVERSE (arg_info->tags, ix, tag)
    if (tag->id)
      bind (tag->id, tag->type, current_scope,
	    /*invisible=*/false, /*nested=*/false, UNKNOWN_LOCATION);
}

/* Subroutine of store_parm_decls which handles old-style function
   definitions (separate parameter list and declarations).  */

static void
store_parm_decls_oldstyle (tree fndecl, const struct c_arg_info *arg_info)
{
  struct c_binding *b;
  tree parm, decl, last;
  tree parmids = arg_info->parms;
  hash_set<tree> seen_args;

  if (!in_system_header_at (input_location))
    warning_at (DECL_SOURCE_LOCATION (fndecl),
		OPT_Wold_style_definition, "old-style function definition");

  /* Match each formal parameter name with its declaration.  Save each
     decl in the appropriate TREE_PURPOSE slot of the parmids chain.  */
  for (parm = parmids; parm; parm = TREE_CHAIN (parm))
    {
      if (TREE_VALUE (parm) == NULL_TREE)
	{
	  error_at (DECL_SOURCE_LOCATION (fndecl),
		    "parameter name missing from parameter list");
	  TREE_PURPOSE (parm) = NULL_TREE;
	  continue;
	}

      b = I_SYMBOL_BINDING (TREE_VALUE (parm));
      if (b && B_IN_CURRENT_SCOPE (b))
	{
	  decl = b->decl;
	  /* Skip erroneous parameters.  */
	  if (decl == error_mark_node)
	    continue;
	  /* If we got something other than a PARM_DECL it is an error.  */
	  if (TREE_CODE (decl) != PARM_DECL)
	    {
	      error_at (DECL_SOURCE_LOCATION (decl),
			"%qD declared as a non-parameter", decl);
	      continue;
	    }
	  /* If the declaration is already marked, we have a duplicate
	     name.  Complain and ignore the duplicate.  */
	  else if (seen_args.contains (decl))
	    {
	      error_at (DECL_SOURCE_LOCATION (decl),
			"multiple parameters named %qD", decl);
	      TREE_PURPOSE (parm) = NULL_TREE;
	      continue;
	    }
	  /* If the declaration says "void", complain and turn it into
	     an int.  */
	  else if (VOID_TYPE_P (TREE_TYPE (decl)))
	    {
	      error_at (DECL_SOURCE_LOCATION (decl),
			"parameter %qD declared with void type", decl);
	      TREE_TYPE (decl) = integer_type_node;
	      DECL_ARG_TYPE (decl) = integer_type_node;
	      layout_decl (decl, 0);
	    }
	  warn_if_shadowing (decl);
	}
      /* If no declaration found, default to int.  */
      else
	{
	  /* FIXME diagnostics: This should be the location of the argument,
	     not the FNDECL.  E.g., for an old-style declaration

	       int f10(v) { blah; }

	     We should use the location of the V, not the F10.
	     Unfortunately, the V is an IDENTIFIER_NODE which has no
	     location.  In the future we need locations for c_arg_info
	     entries.

	     See gcc.dg/Wshadow-3.c for an example of this problem. */
	  decl = build_decl (DECL_SOURCE_LOCATION (fndecl),
			     PARM_DECL, TREE_VALUE (parm), integer_type_node);
	  DECL_ARG_TYPE (decl) = TREE_TYPE (decl);
	  pushdecl (decl);
	  warn_if_shadowing (decl);

	  if (flag_isoc99)
	    pedwarn (DECL_SOURCE_LOCATION (decl),
		     OPT_Wimplicit_int, "type of %qD defaults to %<int%>",
		     decl);
	  else
	    warning_at (DECL_SOURCE_LOCATION (decl),
			OPT_Wmissing_parameter_type,
			"type of %qD defaults to %<int%>", decl);
	}

      TREE_PURPOSE (parm) = decl;
      seen_args.add (decl);
    }

  /* Now examine the parms chain for incomplete declarations
     and declarations with no corresponding names.  */

  for (b = current_scope->bindings; b; b = b->prev)
    {
      parm = b->decl;
      if (TREE_CODE (parm) != PARM_DECL)
	continue;

      if (TREE_TYPE (parm) != error_mark_node
	  && !COMPLETE_TYPE_P (TREE_TYPE (parm)))
	{
	  error_at (DECL_SOURCE_LOCATION (parm),
		    "parameter %qD has incomplete type", parm);
	  TREE_TYPE (parm) = error_mark_node;
	}

      if (!seen_args.contains (parm))
	{
	  error_at (DECL_SOURCE_LOCATION (parm),
		    "declaration for parameter %qD but no such parameter",
		    parm);

	  /* Pretend the parameter was not missing.
	     This gets us to a standard state and minimizes
	     further error messages.  */
	  parmids = chainon (parmids, tree_cons (parm, 0, 0));
	}
    }

  /* Chain the declarations together in the order of the list of
     names.  Store that chain in the function decl, replacing the
     list of names.  Update the current scope to match.  */
  DECL_ARGUMENTS (fndecl) = NULL_TREE;

  for (parm = parmids; parm; parm = TREE_CHAIN (parm))
    if (TREE_PURPOSE (parm))
      break;
  if (parm && TREE_PURPOSE (parm))
    {
      last = TREE_PURPOSE (parm);
      DECL_ARGUMENTS (fndecl) = last;

      for (parm = TREE_CHAIN (parm); parm; parm = TREE_CHAIN (parm))
	if (TREE_PURPOSE (parm))
	  {
	    DECL_CHAIN (last) = TREE_PURPOSE (parm);
	    last = TREE_PURPOSE (parm);
	  }
      DECL_CHAIN (last) = NULL_TREE;
    }

  /* If there was a previous prototype,
     set the DECL_ARG_TYPE of each argument according to
     the type previously specified, and report any mismatches.  */

  if (current_function_prototype_arg_types)
    {
      tree type;
      for (parm = DECL_ARGUMENTS (fndecl),
	     type = current_function_prototype_arg_types;
	   parm || (type != NULL_TREE
		    && TREE_VALUE (type) != error_mark_node
		    && TYPE_MAIN_VARIANT (TREE_VALUE (type)) != void_type_node);
	   parm = DECL_CHAIN (parm), type = TREE_CHAIN (type))
	{
	  if (parm == NULL_TREE
	      || type == NULL_TREE
	      || (TREE_VALUE (type) != error_mark_node
		  && TYPE_MAIN_VARIANT (TREE_VALUE (type)) == void_type_node))
	    {
	      if (current_function_prototype_built_in)
		warning_at (DECL_SOURCE_LOCATION (fndecl),
			    0, "number of arguments doesn%'t match "
			    "built-in prototype");
	      else
		{
		  /* FIXME diagnostics: This should be the location of
		     FNDECL, but there is bug when a prototype is
		     declared inside function context, but defined
		     outside of it (e.g., gcc.dg/pr15698-2.c).  In
		     which case FNDECL gets the location of the
		     prototype, not the definition.  */
		  error_at (input_location,
			    "number of arguments doesn%'t match prototype");

		  error_at (current_function_prototype_locus,
			    "prototype declaration");
		}
	      break;
	    }
	  /* Type for passing arg must be consistent with that
	     declared for the arg.  ISO C says we take the unqualified
	     type for parameters declared with qualified type.  */
	  if (TREE_TYPE (parm) != error_mark_node
	      && TREE_VALUE (type) != error_mark_node
	      && ((TYPE_ATOMIC (DECL_ARG_TYPE (parm))
		   != TYPE_ATOMIC (TREE_VALUE (type)))
		  || !comptypes (TYPE_MAIN_VARIANT (DECL_ARG_TYPE (parm)),
				 TYPE_MAIN_VARIANT (TREE_VALUE (type)))))
	    {
	      if ((TYPE_ATOMIC (DECL_ARG_TYPE (parm))
		   == TYPE_ATOMIC (TREE_VALUE (type)))
		  && (TYPE_MAIN_VARIANT (TREE_TYPE (parm))
		      == TYPE_MAIN_VARIANT (TREE_VALUE (type))))
		{
		  /* Adjust argument to match prototype.  E.g. a previous
		     `int foo(float);' prototype causes
		     `int foo(x) float x; {...}' to be treated like
		     `int foo(float x) {...}'.  This is particularly
		     useful for argument types like uid_t.  */
		  DECL_ARG_TYPE (parm) = TREE_TYPE (parm);

		  if (targetm.calls.promote_prototypes (TREE_TYPE (current_function_decl))
		      && INTEGRAL_TYPE_P (TREE_TYPE (parm))
		      && (TYPE_PRECISION (TREE_TYPE (parm))
			  < TYPE_PRECISION (integer_type_node)))
		    DECL_ARG_TYPE (parm)
		      = c_type_promotes_to (TREE_TYPE (parm));

		  /* ??? Is it possible to get here with a
		     built-in prototype or will it always have
		     been diagnosed as conflicting with an
		     old-style definition and discarded?  */
		  if (current_function_prototype_built_in)
		    warning_at (DECL_SOURCE_LOCATION (parm),
				OPT_Wpedantic, "promoted argument %qD "
				"doesn%'t match built-in prototype", parm);
		  else
		    {
		      pedwarn (DECL_SOURCE_LOCATION (parm),
			       OPT_Wpedantic, "promoted argument %qD "
			       "doesn%'t match prototype", parm);
		      pedwarn (current_function_prototype_locus, OPT_Wpedantic,
			       "prototype declaration");
		    }
		}
	      else
		{
		  if (current_function_prototype_built_in)
		    warning_at (DECL_SOURCE_LOCATION (parm),
				0, "argument %qD doesn%'t match "
				"built-in prototype", parm);
		  else
		    {
		      error_at (DECL_SOURCE_LOCATION (parm),
				"argument %qD doesn%'t match prototype", parm);
		      error_at (current_function_prototype_locus,
				"prototype declaration");
		    }
		}
	    }
	}
      TYPE_ACTUAL_ARG_TYPES (TREE_TYPE (fndecl)) = NULL_TREE;
    }

  /* Otherwise, create a prototype that would match.  */

  else
    {
      tree actual = NULL_TREE, last = NULL_TREE, type;

      for (parm = DECL_ARGUMENTS (fndecl); parm; parm = DECL_CHAIN (parm))
	{
	  type = tree_cons (NULL_TREE, DECL_ARG_TYPE (parm), NULL_TREE);
	  if (last)
	    TREE_CHAIN (last) = type;
	  else
	    actual = type;
	  last = type;
	}
      type = tree_cons (NULL_TREE, void_type_node, NULL_TREE);
      if (last)
	TREE_CHAIN (last) = type;
      else
	actual = type;

      /* We are going to assign a new value for the TYPE_ACTUAL_ARG_TYPES
	 of the type of this function, but we need to avoid having this
	 affect the types of other similarly-typed functions, so we must
	 first force the generation of an identical (but separate) type
	 node for the relevant function type.  The new node we create
	 will be a variant of the main variant of the original function
	 type.  */

      TREE_TYPE (fndecl) = build_variant_type_copy (TREE_TYPE (fndecl));

      TYPE_ACTUAL_ARG_TYPES (TREE_TYPE (fndecl)) = actual;
    }
}

/* Store parameter declarations passed in ARG_INFO into the current
   function declaration.  */

void
store_parm_decls_from (struct c_arg_info *arg_info)
{
  current_function_arg_info = arg_info;
  store_parm_decls ();
}

/* Called by walk_tree to look for and update context-less labels.  */

static tree
set_labels_context_r (tree *tp, int *walk_subtrees, void *data)
{
  if (TREE_CODE (*tp) == LABEL_EXPR
      && DECL_CONTEXT (LABEL_EXPR_LABEL (*tp)) == NULL_TREE)
    {
      DECL_CONTEXT (LABEL_EXPR_LABEL (*tp)) = static_cast<tree>(data);
      *walk_subtrees = 0;
    }

  return NULL_TREE;
}

/* Store the parameter declarations into the current function declaration.
   This is called after parsing the parameter declarations, before
   digesting the body of the function.

   For an old-style definition, construct a prototype out of the old-style
   parameter declarations and inject it into the function's type.  */

void
store_parm_decls (void)
{
  tree fndecl = current_function_decl;
  bool proto;

  /* The argument information block for FNDECL.  */
  struct c_arg_info *arg_info = current_function_arg_info;
  current_function_arg_info = 0;

  /* True if this definition is written with a prototype.  Note:
     despite C99 6.7.5.3p14, we can *not* treat an empty argument
     list in a function definition as equivalent to (void) -- an
     empty argument list specifies the function has no parameters,
     but only (void) sets up a prototype for future calls.  */
  proto = arg_info->types != 0;

  if (proto)
    store_parm_decls_newstyle (fndecl, arg_info);
  else
    store_parm_decls_oldstyle (fndecl, arg_info);

  /* The next call to push_scope will be a function body.  */

  next_is_function_body = true;

  /* Write a record describing this function definition to the prototypes
     file (if requested).  */

  gen_aux_info_record (fndecl, 1, 0, proto);

  /* Initialize the RTL code for the function.  */
  allocate_struct_function (fndecl, false);

  if (warn_unused_local_typedefs)
    cfun->language = ggc_cleared_alloc<language_function> ();

  /* Begin the statement tree for this function.  */
  DECL_SAVED_TREE (fndecl) = push_stmt_list ();

  /* ??? Insert the contents of the pending sizes list into the function
     to be evaluated.  The only reason left to have this is
	void foo(int n, int array[n++])
     because we throw away the array type in favor of a pointer type, and
     thus won't naturally see the SAVE_EXPR containing the increment.  All
     other pending sizes would be handled by gimplify_parameters.  */
  if (arg_info->pending_sizes)
    {
      /* In very special circumstances, e.g. for code like
	   _Atomic int i = 5;
	   void f (int a[i += 2]) {}
	 we need to execute the atomic assignment on function entry.
	 But in this case, it is not just a straight store, it has the
	 op= form, which means that build_atomic_assign has generated
	 gotos, labels, etc.  Because at that time the function decl
	 for F has not been created yet, those labels do not have any
	 function context.  But we have the fndecl now, so update the
	 labels accordingly.  gimplify_expr would crash otherwise.  */
      walk_tree_without_duplicates (&arg_info->pending_sizes,
				    set_labels_context_r, fndecl);
      add_stmt (arg_info->pending_sizes);
    }
}

/* Store PARM_DECLs in PARMS into scope temporarily.  Used for
   c_finish_omp_declare_simd for function prototypes.  No diagnostics
   should be done.  */

void
temp_store_parm_decls (tree fndecl, tree parms)
{
  push_scope ();
  for (tree p = parms; p; p = DECL_CHAIN (p))
    {
      DECL_CONTEXT (p) = fndecl;
      if (DECL_NAME (p))
	bind (DECL_NAME (p), p, current_scope,
	      /*invisible=*/false, /*nested=*/false,
	      UNKNOWN_LOCATION);
    }
}

/* Undo what temp_store_parm_decls did.  */

void
temp_pop_parm_decls (void)
{
  /* Clear all bindings in this temporary scope, so that
     pop_scope doesn't create a BLOCK.  */
  struct c_binding *b = current_scope->bindings;
  current_scope->bindings = NULL;
  for (; b; b = free_binding_and_advance (b))
    {
      gcc_assert (TREE_CODE (b->decl) == PARM_DECL
		  || b->decl == error_mark_node);
      gcc_assert (I_SYMBOL_BINDING (b->id) == b);
      I_SYMBOL_BINDING (b->id) = b->shadowed;
      if (b->shadowed && b->shadowed->u.type)
	TREE_TYPE (b->shadowed->decl) = b->shadowed->u.type;
    }
  pop_scope ();
}


/* Finish up a function declaration and compile that function
   all the way to assembler language output.  Then free the storage
   for the function definition.

   This is called after parsing the body of the function definition.  */

void
finish_function (void)
{
  tree fndecl = current_function_decl;
  
  if (c_dialect_objc ())
    objc_finish_function ();

  if (TREE_CODE (fndecl) == FUNCTION_DECL
      && targetm.calls.promote_prototypes (TREE_TYPE (fndecl)))
    {
      tree args = DECL_ARGUMENTS (fndecl);
      for (; args; args = DECL_CHAIN (args))
	{
	  tree type = TREE_TYPE (args);
	  if (INTEGRAL_TYPE_P (type)
	      && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
	    DECL_ARG_TYPE (args) = c_type_promotes_to (type);
	}
    }

  if (DECL_INITIAL (fndecl) && DECL_INITIAL (fndecl) != error_mark_node)
    BLOCK_SUPERCONTEXT (DECL_INITIAL (fndecl)) = fndecl;

  /* Must mark the RESULT_DECL as being in this function.  */

  if (DECL_RESULT (fndecl) && DECL_RESULT (fndecl) != error_mark_node)
    DECL_CONTEXT (DECL_RESULT (fndecl)) = fndecl;

  if (MAIN_NAME_P (DECL_NAME (fndecl)) && flag_hosted
      && TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (fndecl)))
      == integer_type_node && flag_isoc99)
    {
      /* Hack.  We don't want the middle-end to warn that this return
	 is unreachable, so we mark its location as special.  Using
	 UNKNOWN_LOCATION has the problem that it gets clobbered in
	 annotate_one_with_locus.  A cleaner solution might be to
	 ensure ! should_carry_locus_p (stmt), but that needs a flag.
      */
      c_finish_return (BUILTINS_LOCATION, integer_zero_node, NULL_TREE);
    }

  /* Tie off the statement tree for this function.  */
  DECL_SAVED_TREE (fndecl) = pop_stmt_list (DECL_SAVED_TREE (fndecl));

  /* If the function has _Cilk_spawn in front of a function call inside it
     i.e. it is a spawning function, then add the appropriate Cilk plus
     functions inside.  */
  if (fn_contains_cilk_spawn_p (cfun))
    cfun->cilk_frame_decl = insert_cilk_frame (fndecl);

  finish_fname_decls ();

  /* Complain if there's just no return statement.  */
  if (warn_return_type
      && TREE_CODE (TREE_TYPE (TREE_TYPE (fndecl))) != VOID_TYPE
      && !current_function_returns_value && !current_function_returns_null
      /* Don't complain if we are no-return.  */
      && !current_function_returns_abnormally
      /* Don't complain if we are declared noreturn.  */
      && !TREE_THIS_VOLATILE (fndecl)
      /* Don't warn for main().  */
      && !MAIN_NAME_P (DECL_NAME (fndecl))
      /* Or if they didn't actually specify a return type.  */
      && !C_FUNCTION_IMPLICIT_INT (fndecl)
      /* Normally, with -Wreturn-type, flow will complain, but we might
         optimize out static functions.  */
      && !TREE_PUBLIC (fndecl))
    {
      warning (OPT_Wreturn_type,
	       "no return statement in function returning non-void");
      TREE_NO_WARNING (fndecl) = 1;
    }

  /* Complain about parameters that are only set, but never otherwise used.  */
  if (warn_unused_but_set_parameter)
    {
      tree decl;

      for (decl = DECL_ARGUMENTS (fndecl);
	   decl;
	   decl = DECL_CHAIN (decl))
	if (TREE_USED (decl)
	    && TREE_CODE (decl) == PARM_DECL
	    && !DECL_READ_P (decl)
	    && DECL_NAME (decl)
	    && !DECL_ARTIFICIAL (decl)
	    && !TREE_NO_WARNING (decl))
	  warning_at (DECL_SOURCE_LOCATION (decl),
		      OPT_Wunused_but_set_parameter,
		      "parameter %qD set but not used", decl);
    }

  /* Complain about locally defined typedefs that are not used in this
     function.  */
  maybe_warn_unused_local_typedefs ();

  /* Possibly warn about unused parameters.  */
  if (warn_unused_parameter)
    do_warn_unused_parameter (fndecl);

  /* Store the end of the function, so that we get good line number
     info for the epilogue.  */
  cfun->function_end_locus = input_location;

  /* Finalize the ELF visibility for the function.  */
  c_determine_visibility (fndecl);

  /* For GNU C extern inline functions disregard inline limits.  */
  if (DECL_EXTERNAL (fndecl)
      && DECL_DECLARED_INLINE_P (fndecl)
      && (flag_gnu89_inline
	  || lookup_attribute ("gnu_inline", DECL_ATTRIBUTES (fndecl))))
    DECL_DISREGARD_INLINE_LIMITS (fndecl) = 1;

  /* Genericize before inlining.  Delay genericizing nested functions
     until their parent function is genericized.  Since finalizing
     requires GENERIC, delay that as well.  */

  if (DECL_INITIAL (fndecl) && DECL_INITIAL (fndecl) != error_mark_node
      && !undef_nested_function)
    {
      if (!decl_function_context (fndecl))
	{
	  invoke_plugin_callbacks (PLUGIN_PRE_GENERICIZE, fndecl);
	  c_genericize (fndecl);

	  /* ??? Objc emits functions after finalizing the compilation unit.
	     This should be cleaned up later and this conditional removed.  */
	  if (symtab->global_info_ready)
	    {
	      cgraph_node::add_new_function (fndecl, false);
	      return;
	    }
	  cgraph_node::finalize_function (fndecl, false);
	}
      else
	{
	  /* Register this function with cgraph just far enough to get it
	    added to our parent's nested function list.  Handy, since the
	    C front end doesn't have such a list.  */
	  (void) cgraph_node::get_create (fndecl);
	}
    }

  if (!decl_function_context (fndecl))
    undef_nested_function = false;

  if (cfun->language != NULL)
    {
      ggc_free (cfun->language);
      cfun->language = NULL;
    }

  /* We're leaving the context of this function, so zap cfun.
     It's still in DECL_STRUCT_FUNCTION, and we'll restore it in
     tree_rest_of_compilation.  */
  set_cfun (NULL);
  invoke_plugin_callbacks (PLUGIN_FINISH_PARSE_FUNCTION, current_function_decl);
  current_function_decl = NULL;
}

/* Check the declarations given in a for-loop for satisfying the C99
   constraints.  If exactly one such decl is found, return it.  LOC is
   the location of the opening parenthesis of the for loop.  The last
   parameter allows you to control the "for loop initial declarations
   are only allowed in C99 mode".  Normally, you should pass
   flag_isoc99 as that parameter.  But in some cases (Objective-C
   foreach loop, for example) we want to run the checks in this
   function even if not in C99 mode, so we allow the caller to turn
   off the error about not being in C99 mode.
*/

tree
check_for_loop_decls (location_t loc, bool turn_off_iso_c99_error)
{
  struct c_binding *b;
  tree one_decl = NULL_TREE;
  int n_decls = 0;

  if (!turn_off_iso_c99_error)
    {
      static bool hint = true;
      /* If we get here, declarations have been used in a for loop without
	 the C99 for loop scope.  This doesn't make much sense, so don't
	 allow it.  */
      error_at (loc, "%<for%> loop initial declarations "
		"are only allowed in C99 or C11 mode");
      if (hint)
	{
	  inform (loc,
		  "use option -std=c99, -std=gnu99, -std=c11 or -std=gnu11 "
		  "to compile your code");
	  hint = false;
	}
      return NULL_TREE;
    }
  /* C99 subclause 6.8.5 paragraph 3:

       [#3]  The  declaration  part  of  a for statement shall only
       declare identifiers for objects having storage class auto or
       register.

     It isn't clear whether, in this sentence, "identifiers" binds to
     "shall only declare" or to "objects" - that is, whether all identifiers
     declared must be identifiers for objects, or whether the restriction
     only applies to those that are.  (A question on this in comp.std.c
     in November 2000 received no answer.)  We implement the strictest
     interpretation, to avoid creating an extension which later causes
     problems.  */

  for (b = current_scope->bindings; b; b = b->prev)
    {
      tree id = b->id;
      tree decl = b->decl;

      if (!id)
	continue;

      switch (TREE_CODE (decl))
	{
	case VAR_DECL:
	  {
	    location_t decl_loc = DECL_SOURCE_LOCATION (decl);
	    if (TREE_STATIC (decl))
	      error_at (decl_loc,
			"declaration of static variable %qD in %<for%> loop "
			"initial declaration", decl);
	    else if (DECL_EXTERNAL (decl))
	      error_at (decl_loc,
			"declaration of %<extern%> variable %qD in %<for%> loop "
			"initial declaration", decl);
	  }
	  break;

	case RECORD_TYPE:
	  error_at (loc,
		    "%<struct %E%> declared in %<for%> loop initial "
		    "declaration", id);
	  break;
	case UNION_TYPE:
	  error_at (loc,
		    "%<union %E%> declared in %<for%> loop initial declaration",
		    id);
	  break;
	case ENUMERAL_TYPE:
	  error_at (loc, "%<enum %E%> declared in %<for%> loop "
		    "initial declaration", id);
	  break;
	default:
	  error_at (loc, "declaration of non-variable "
		    "%qD in %<for%> loop initial declaration", decl);
	}

      n_decls++;
      one_decl = decl;
    }

  return n_decls == 1 ? one_decl : NULL_TREE;
}

/* Save and reinitialize the variables
   used during compilation of a C function.  */

void
c_push_function_context (void)
{
  struct language_function *p = cfun->language;
  /* cfun->language might have been already allocated by the use of
     -Wunused-local-typedefs.  In that case, just re-use it.  */
  if (p == NULL)
    cfun->language = p = ggc_cleared_alloc<language_function> ();

  p->base.x_stmt_tree = c_stmt_tree;
  c_stmt_tree.x_cur_stmt_list = vec_safe_copy (c_stmt_tree.x_cur_stmt_list);
  p->x_break_label = c_break_label;
  p->x_cont_label = c_cont_label;
  p->x_switch_stack = c_switch_stack;
  p->arg_info = current_function_arg_info;
  p->returns_value = current_function_returns_value;
  p->returns_null = current_function_returns_null;
  p->returns_abnormally = current_function_returns_abnormally;
  p->warn_about_return_type = warn_about_return_type;

  push_function_context ();
}

/* Restore the variables used during compilation of a C function.  */

void
c_pop_function_context (void)
{
  struct language_function *p;

  pop_function_context ();
  p = cfun->language;

  /* When -Wunused-local-typedefs is in effect, cfun->languages is
     used to store data throughout the life time of the current cfun,
     So don't deallocate it.  */
  if (!warn_unused_local_typedefs)
    cfun->language = NULL;

  if (DECL_STRUCT_FUNCTION (current_function_decl) == 0
      && DECL_SAVED_TREE (current_function_decl) == NULL_TREE)
    {
      /* Stop pointing to the local nodes about to be freed.  */
      /* But DECL_INITIAL must remain nonzero so we know this
	 was an actual function definition.  */
      DECL_INITIAL (current_function_decl) = error_mark_node;
      DECL_ARGUMENTS (current_function_decl) = NULL_TREE;
    }

  c_stmt_tree = p->base.x_stmt_tree;
  p->base.x_stmt_tree.x_cur_stmt_list = NULL;
  c_break_label = p->x_break_label;
  c_cont_label = p->x_cont_label;
  c_switch_stack = p->x_switch_stack;
  current_function_arg_info = p->arg_info;
  current_function_returns_value = p->returns_value;
  current_function_returns_null = p->returns_null;
  current_function_returns_abnormally = p->returns_abnormally;
  warn_about_return_type = p->warn_about_return_type;
}

/* The functions below are required for functionality of doing
   function at once processing in the C front end. Currently these
   functions are not called from anywhere in the C front end, but as
   these changes continue, that will change.  */

/* Returns the stmt_tree (if any) to which statements are currently
   being added.  If there is no active statement-tree, NULL is
   returned.  */

stmt_tree
current_stmt_tree (void)
{
  return &c_stmt_tree;
}

/* Return the global value of T as a symbol.  */

tree
identifier_global_value	(tree t)
{
  struct c_binding *b;

  for (b = I_SYMBOL_BINDING (t); b; b = b->shadowed)
    if (B_IN_FILE_SCOPE (b) || B_IN_EXTERNAL_SCOPE (b))
      return b->decl;

  return NULL_TREE;
}

/* In C, the only C-linkage public declaration is at file scope.  */

tree
c_linkage_bindings (tree name)
{
  return identifier_global_value (name);
}

/* Record a builtin type for C.  If NAME is non-NULL, it is the name used;
   otherwise the name is found in ridpointers from RID_INDEX.  */

void
record_builtin_type (enum rid rid_index, const char *name, tree type)
{
  tree id, decl;
  if (name == 0)
    id = ridpointers[(int) rid_index];
  else
    id = get_identifier (name);
  decl = build_decl (UNKNOWN_LOCATION, TYPE_DECL, id, type);
  pushdecl (decl);
  if (debug_hooks->type_decl)
    debug_hooks->type_decl (decl, false);
}

/* Build the void_list_node (void_type_node having been created).  */
tree
build_void_list_node (void)
{
  tree t = build_tree_list (NULL_TREE, void_type_node);
  return t;
}

/* Return a c_parm structure with the given SPECS, ATTRS and DECLARATOR.  */

struct c_parm *
build_c_parm (struct c_declspecs *specs, tree attrs,
	      struct c_declarator *declarator,
	      location_t loc)
{
  struct c_parm *ret = XOBNEW (&parser_obstack, struct c_parm);
  ret->specs = specs;
  ret->attrs = attrs;
  ret->declarator = declarator;
  ret->loc = loc;
  return ret;
}

/* Return a declarator with nested attributes.  TARGET is the inner
   declarator to which these attributes apply.  ATTRS are the
   attributes.  */

struct c_declarator *
build_attrs_declarator (tree attrs, struct c_declarator *target)
{
  struct c_declarator *ret = XOBNEW (&parser_obstack, struct c_declarator);
  ret->kind = cdk_attrs;
  ret->declarator = target;
  ret->u.attrs = attrs;
  return ret;
}

/* Return a declarator for a function with arguments specified by ARGS
   and return type specified by TARGET.  */

struct c_declarator *
build_function_declarator (struct c_arg_info *args,
			   struct c_declarator *target)
{
  struct c_declarator *ret = XOBNEW (&parser_obstack, struct c_declarator);
  ret->kind = cdk_function;
  ret->declarator = target;
  ret->u.arg_info = args;
  return ret;
}

/* Return a declarator for the identifier IDENT (which may be
   NULL_TREE for an abstract declarator).  */

struct c_declarator *
build_id_declarator (tree ident)
{
  struct c_declarator *ret = XOBNEW (&parser_obstack, struct c_declarator);
  ret->kind = cdk_id;
  ret->declarator = 0;
  ret->u.id = ident;
  /* Default value - may get reset to a more precise location. */
  ret->id_loc = input_location;
  return ret;
}

/* Return something to represent absolute declarators containing a *.
   TARGET is the absolute declarator that the * contains.
   TYPE_QUALS_ATTRS is a structure for type qualifiers and attributes
   to apply to the pointer type.  */

struct c_declarator *
make_pointer_declarator (struct c_declspecs *type_quals_attrs,
			 struct c_declarator *target)
{
  tree attrs;
  int quals = 0;
  struct c_declarator *itarget = target;
  struct c_declarator *ret = XOBNEW (&parser_obstack, struct c_declarator);
  if (type_quals_attrs)
    {
      attrs = type_quals_attrs->attrs;
      quals = quals_from_declspecs (type_quals_attrs);
      if (attrs != NULL_TREE)
	itarget = build_attrs_declarator (attrs, target);
    }
  ret->kind = cdk_pointer;
  ret->declarator = itarget;
  ret->u.pointer_quals = quals;
  return ret;
}

/* Return a pointer to a structure for an empty list of declaration
   specifiers.  */

struct c_declspecs *
build_null_declspecs (void)
{
  struct c_declspecs *ret = XOBNEW (&parser_obstack, struct c_declspecs);
  memset (ret, 0, sizeof *ret);
  ret->align_log = -1;
  ret->typespec_word = cts_none;
  ret->storage_class = csc_none;
  ret->expr_const_operands = true;
  ret->typespec_kind = ctsk_none;
  ret->address_space = ADDR_SPACE_GENERIC;
  return ret;
}

/* Add the address space ADDRSPACE to the declaration specifiers
   SPECS, returning SPECS.  */

struct c_declspecs *
declspecs_add_addrspace (source_location location,
			 struct c_declspecs *specs, addr_space_t as)
{
  specs->non_sc_seen_p = true;
  specs->declspecs_seen_p = true;

  if (!ADDR_SPACE_GENERIC_P (specs->address_space)
      && specs->address_space != as)
    error ("incompatible address space qualifiers %qs and %qs",
	   c_addr_space_name (as),
	   c_addr_space_name (specs->address_space));
  else
    {
      specs->address_space = as;
      specs->locations[cdw_address_space] = location;
    }
  return specs;
}

/* Add the type qualifier QUAL to the declaration specifiers SPECS,
   returning SPECS.  */

struct c_declspecs *
declspecs_add_qual (source_location loc,
		    struct c_declspecs *specs, tree qual)
{
  enum rid i;
  bool dupe = false;
  specs->non_sc_seen_p = true;
  specs->declspecs_seen_p = true;
  gcc_assert (TREE_CODE (qual) == IDENTIFIER_NODE
	      && C_IS_RESERVED_WORD (qual));
  i = C_RID_CODE (qual);
  location_t prev_loc = UNKNOWN_LOCATION;
  switch (i)
    {
    case RID_CONST:
      dupe = specs->const_p;
      specs->const_p = true;
      prev_loc = specs->locations[cdw_const];
      specs->locations[cdw_const] = loc;
      break;
    case RID_VOLATILE:
      dupe = specs->volatile_p;
      specs->volatile_p = true;
      prev_loc = specs->locations[cdw_volatile];
      specs->locations[cdw_volatile] = loc;
      break;
    case RID_RESTRICT:
      dupe = specs->restrict_p;
      specs->restrict_p = true;
      prev_loc = specs->locations[cdw_restrict];
      specs->locations[cdw_restrict] = loc;
      break;
    case RID_ATOMIC:
      dupe = specs->atomic_p;
      specs->atomic_p = true;
      prev_loc = specs->locations[cdw_atomic];
      specs->locations[cdw_atomic] = loc;
      break;
    default:
      gcc_unreachable ();
    }
  if (dupe)
    {
      bool warned = pedwarn_c90 (loc, OPT_Wpedantic,
				 "duplicate %qE declaration specifier", qual);
      if (!warned
	  && warn_duplicate_decl_specifier
	  && prev_loc >= RESERVED_LOCATION_COUNT
	  && !from_macro_expansion_at (prev_loc)
	  && !from_macro_expansion_at (loc))
	warning_at (loc, OPT_Wduplicate_decl_specifier,
		    "duplicate %qE declaration specifier", qual);
    }
  return specs;
}

/* Add the type specifier TYPE to the declaration specifiers SPECS,
   returning SPECS.  */

struct c_declspecs *
declspecs_add_type (location_t loc, struct c_declspecs *specs,
		    struct c_typespec spec)
{
  tree type = spec.spec;
  specs->non_sc_seen_p = true;
  specs->declspecs_seen_p = true;
  specs->typespec_kind = spec.kind;
  if (TREE_DEPRECATED (type))
    specs->deprecated_p = true;

  /* Handle type specifier keywords.  */
  if (TREE_CODE (type) == IDENTIFIER_NODE
      && C_IS_RESERVED_WORD (type)
      && C_RID_CODE (type) != RID_CXX_COMPAT_WARN)
    {
      enum rid i = C_RID_CODE (type);
      if (specs->type)
	{
	  error_at (loc, "two or more data types in declaration specifiers");
	  return specs;
	}
      if ((int) i <= (int) RID_LAST_MODIFIER)
	{
	  /* "long", "short", "signed", "unsigned", "_Complex" or "_Sat".  */
	  bool dupe = false;
	  switch (i)
	    {
	    case RID_LONG:
	      if (specs->long_long_p)
		{
		  error_at (loc, "%<long long long%> is too long for GCC");
		  break;
		}
	      if (specs->long_p)
		{
		  if (specs->typespec_word == cts_double)
		    {
		      error_at (loc,
				("both %<long long%> and %<double%> in "
				 "declaration specifiers"));
		      break;
		    }
		  pedwarn_c90 (loc, OPT_Wlong_long,
			       "ISO C90 does not support %<long long%>");
		  specs->long_long_p = 1;
		  specs->locations[cdw_long_long] = loc;
		  break;
		}
	      if (specs->short_p)
		error_at (loc,
			  ("both %<long%> and %<short%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_auto_type)
		error_at (loc,
			  ("both %<long%> and %<__auto_type%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_void)
		error_at (loc,
			  ("both %<long%> and %<void%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_int_n)
		  error_at (loc,
			    ("both %<long%> and %<__int%d%> in "
			     "declaration specifiers"),
			    int_n_data[specs->int_n_idx].bitsize);
	      else if (specs->typespec_word == cts_bool)
		error_at (loc,
			  ("both %<long%> and %<_Bool%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_char)
		error_at (loc,
			  ("both %<long%> and %<char%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_float)
		error_at (loc,
			  ("both %<long%> and %<float%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_floatn_nx)
		error_at (loc,
			  ("both %<long%> and %<_Float%d%s%> in "
			   "declaration specifiers"),
			  floatn_nx_types[specs->floatn_nx_idx].n,
			  (floatn_nx_types[specs->floatn_nx_idx].extended
			   ? "x"
			   : ""));
	      else if (specs->typespec_word == cts_dfloat32)
		error_at (loc,
			  ("both %<long%> and %<_Decimal32%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_dfloat64)
		error_at (loc,
			  ("both %<long%> and %<_Decimal64%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_dfloat128)
		error_at (loc,
			  ("both %<long%> and %<_Decimal128%> in "
			   "declaration specifiers"));
	      else
		{
		  specs->long_p = true;
		  specs->locations[cdw_long] = loc;
		}
	      break;
	    case RID_SHORT:
	      dupe = specs->short_p;
	      if (specs->long_p)
		error_at (loc,
			  ("both %<long%> and %<short%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_auto_type)
		error_at (loc,
			  ("both %<short%> and %<__auto_type%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_void)
		error_at (loc,
			  ("both %<short%> and %<void%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_int_n)
		error_at (loc,
			  ("both %<short%> and %<__int%d%> in "
			   "declaration specifiers"),
			  int_n_data[specs->int_n_idx].bitsize);
	      else if (specs->typespec_word == cts_bool)
		error_at (loc,
			  ("both %<short%> and %<_Bool%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_char)
		error_at (loc,
			  ("both %<short%> and %<char%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_float)
		error_at (loc,
			  ("both %<short%> and %<float%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_double)
		error_at (loc,
			  ("both %<short%> and %<double%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_floatn_nx)
		error_at (loc,
			  ("both %<short%> and %<_Float%d%s%> in "
			   "declaration specifiers"),
			  floatn_nx_types[specs->floatn_nx_idx].n,
			  (floatn_nx_types[specs->floatn_nx_idx].extended
			   ? "x"
			   : ""));
	      else if (specs->typespec_word == cts_dfloat32)
                error_at (loc,
			  ("both %<short%> and %<_Decimal32%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_dfloat64)
		error_at (loc,
			  ("both %<short%> and %<_Decimal64%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_dfloat128)
		error_at (loc,
			  ("both %<short%> and %<_Decimal128%> in "
			   "declaration specifiers"));
	      else
		{
		  specs->short_p = true;
		  specs->locations[cdw_short] = loc;
		}
	      break;
	    case RID_SIGNED:
	      dupe = specs->signed_p;
	      if (specs->unsigned_p)
		error_at (loc,
			  ("both %<signed%> and %<unsigned%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_auto_type)
		error_at (loc,
			  ("both %<signed%> and %<__auto_type%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_void)
		error_at (loc,
			  ("both %<signed%> and %<void%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_bool)
		error_at (loc,
			  ("both %<signed%> and %<_Bool%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_float)
		error_at (loc,
			  ("both %<signed%> and %<float%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_double)
		error_at (loc,
			  ("both %<signed%> and %<double%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_floatn_nx)
		error_at (loc,
			  ("both %<signed%> and %<_Float%d%s%> in "
			   "declaration specifiers"),
			  floatn_nx_types[specs->floatn_nx_idx].n,
			  (floatn_nx_types[specs->floatn_nx_idx].extended
			   ? "x"
			   : ""));
	      else if (specs->typespec_word == cts_dfloat32)
		error_at (loc,
			  ("both %<signed%> and %<_Decimal32%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_dfloat64)
		error_at (loc,
			  ("both %<signed%> and %<_Decimal64%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_dfloat128)
		error_at (loc,
			  ("both %<signed%> and %<_Decimal128%> in "
			   "declaration specifiers"));
	      else
		{
		  specs->signed_p = true;
		  specs->locations[cdw_signed] = loc;
		}
	      break;
	    case RID_UNSIGNED:
	      dupe = specs->unsigned_p;
	      if (specs->signed_p)
		error_at (loc,
			  ("both %<signed%> and %<unsigned%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_auto_type)
		error_at (loc,
			  ("both %<unsigned%> and %<__auto_type%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_void)
		error_at (loc,
			  ("both %<unsigned%> and %<void%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_bool)
		error_at (loc,
			  ("both %<unsigned%> and %<_Bool%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_float)
		error_at (loc,
			  ("both %<unsigned%> and %<float%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_double)
		error_at (loc,
			  ("both %<unsigned%> and %<double%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_floatn_nx)
		error_at (loc,
			  ("both %<unsigned%> and %<_Float%d%s%> in "
			   "declaration specifiers"),
			  floatn_nx_types[specs->floatn_nx_idx].n,
			  (floatn_nx_types[specs->floatn_nx_idx].extended
			   ? "x"
			   : ""));
              else if (specs->typespec_word == cts_dfloat32)
		error_at (loc,
			  ("both %<unsigned%> and %<_Decimal32%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_dfloat64)
		error_at (loc,
			  ("both %<unsigned%> and %<_Decimal64%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_dfloat128)
		error_at (loc,
			  ("both %<unsigned%> and %<_Decimal128%> in "
			   "declaration specifiers"));
	      else
		{
		  specs->unsigned_p = true;
		  specs->locations[cdw_unsigned] = loc;
		}
	      break;
	    case RID_COMPLEX:
	      dupe = specs->complex_p;
	      if (!in_system_header_at (loc))
		pedwarn_c90 (loc, OPT_Wpedantic,
			     "ISO C90 does not support complex types");
	      if (specs->typespec_word == cts_auto_type)
		error_at (loc,
			  ("both %<complex%> and %<__auto_type%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_void)
		error_at (loc,
			  ("both %<complex%> and %<void%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_bool)
		error_at (loc,
			  ("both %<complex%> and %<_Bool%> in "
			   "declaration specifiers"));
              else if (specs->typespec_word == cts_dfloat32)
		error_at (loc,
			  ("both %<complex%> and %<_Decimal32%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_dfloat64)
		error_at (loc,
			  ("both %<complex%> and %<_Decimal64%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_dfloat128)
		error_at (loc,
			  ("both %<complex%> and %<_Decimal128%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_fract)
		error_at (loc,
			  ("both %<complex%> and %<_Fract%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_accum)
		error_at (loc,
			  ("both %<complex%> and %<_Accum%> in "
			   "declaration specifiers"));
	      else if (specs->saturating_p)
		error_at (loc,
			  ("both %<complex%> and %<_Sat%> in "
			   "declaration specifiers"));
	      else
		{
		  specs->complex_p = true;
		  specs->locations[cdw_complex] = loc;
		}
	      break;
	    case RID_SAT:
	      dupe = specs->saturating_p;
	      pedwarn (loc, OPT_Wpedantic,
		       "ISO C does not support saturating types");
	      if (specs->typespec_word == cts_int_n)
	        {
		  error_at (loc,
			    ("both %<_Sat%> and %<__int%d%> in "
			     "declaration specifiers"),
			    int_n_data[specs->int_n_idx].bitsize);
	        }
	      else if (specs->typespec_word == cts_auto_type)
		error_at (loc,
			  ("both %<_Sat%> and %<__auto_type%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_void)
		error_at (loc,
			  ("both %<_Sat%> and %<void%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_bool)
		error_at (loc,
			  ("both %<_Sat%> and %<_Bool%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_char)
		error_at (loc,
			  ("both %<_Sat%> and %<char%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_int)
		error_at (loc,
			  ("both %<_Sat%> and %<int%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_float)
		error_at (loc,
			  ("both %<_Sat%> and %<float%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_double)
		error_at (loc,
			  ("both %<_Sat%> and %<double%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_floatn_nx)
		error_at (loc,
			  ("both %<_Sat%> and %<_Float%d%s%> in "
			   "declaration specifiers"),
			  floatn_nx_types[specs->floatn_nx_idx].n,
			  (floatn_nx_types[specs->floatn_nx_idx].extended
			   ? "x"
			   : ""));
              else if (specs->typespec_word == cts_dfloat32)
		error_at (loc,
			  ("both %<_Sat%> and %<_Decimal32%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_dfloat64)
		error_at (loc,
			  ("both %<_Sat%> and %<_Decimal64%> in "
			   "declaration specifiers"));
	      else if (specs->typespec_word == cts_dfloat128)
		error_at (loc,
			  ("both %<_Sat%> and %<_Decimal128%> in "
			   "declaration specifiers"));
	      else if (specs->complex_p)
		error_at (loc,
			  ("both %<_Sat%> and %<complex%> in "
			   "declaration specifiers"));
	      else
		{
		  specs->saturating_p = true;
		  specs->locations[cdw_saturating] = loc;
		}
	      break;
	    default:
	      gcc_unreachable ();
	    }

	  if (dupe)
	    error_at (loc, "duplicate %qE", type);

	  return specs;
	}
      else
	{
	  /* "void", "_Bool", "char", "int", "float", "double",
	     "_FloatN", "_FloatNx", "_Decimal32", "__intN",
	     "_Decimal64", "_Decimal128", "_Fract", "_Accum" or
	     "__auto_type".  */
	  if (specs->typespec_word != cts_none)
	    {
	      error_at (loc,
			"two or more data types in declaration specifiers");
	      return specs;
	    }
	  switch (i)
	    {
	    case RID_AUTO_TYPE:
	      if (specs->long_p)
		error_at (loc,
			  ("both %<long%> and %<__auto_type%> in "
			   "declaration specifiers"));
	      else if (specs->short_p)
		error_at (loc,
			  ("both %<short%> and %<__auto_type%> in "
			   "declaration specifiers"));
	      else if (specs->signed_p)
		error_at (loc,
			  ("both %<signed%> and %<__auto_type%> in "
			   "declaration specifiers"));
	      else if (specs->unsigned_p)
		error_at (loc,
			  ("both %<unsigned%> and %<__auto_type%> in "
			   "declaration specifiers"));
	      else if (specs->complex_p)
		error_at (loc,
			  ("both %<complex%> and %<__auto_type%> in "
			   "declaration specifiers"));
	      else if (specs->saturating_p)
		error_at (loc,
			  ("both %<_Sat%> and %<__auto_type%> in "
			   "declaration specifiers"));
	      else
		{
		  specs->typespec_word = cts_auto_type;
		  specs->locations[cdw_typespec] = loc;
		}
	      return specs;
	    case RID_INT_N_0:
	    case RID_INT_N_1:
	    case RID_INT_N_2:
	    case RID_INT_N_3:
	      specs->int_n_idx = i - RID_INT_N_0;
	      if (!in_system_header_at (input_location))
		pedwarn (loc, OPT_Wpedantic,
			 "ISO C does not support %<__int%d%> types",
			 int_n_data[specs->int_n_idx].bitsize);

	      if (specs->long_p)
		error_at (loc,
			  ("both %<__int%d%> and %<long%> in "
			   "declaration specifiers"),
			  int_n_data[specs->int_n_idx].bitsize);
	      else if (specs->saturating_p)
		error_at (loc,
			  ("both %<_Sat%> and %<__int%d%> in "
			   "declaration specifiers"),
			  int_n_data[specs->int_n_idx].bitsize);
	      else if (specs->short_p)
		error_at (loc,
			  ("both %<__int%d%> and %<short%> in "
			   "declaration specifiers"),
			  int_n_data[specs->int_n_idx].bitsize);
	      else if (! int_n_enabled_p[specs->int_n_idx])
		{
		  specs->typespec_word = cts_int_n;
		  error_at (loc,
			    "%<__int%d%> is not supported on this target",
			    int_n_data[specs->int_n_idx].bitsize);
		}
	      else
		{
		  specs->typespec_word = cts_int_n;
		  specs->locations[cdw_typespec] = loc;
		}
	      return specs;
	    case RID_VOID:
	      if (specs->long_p)
		error_at (loc,
			  ("both %<long%> and %<void%> in "
			   "declaration specifiers"));
	      else if (specs->short_p)
		error_at (loc,
			  ("both %<short%> and %<void%> in "
			   "declaration specifiers"));
	      else if (specs->signed_p)
		error_at (loc,
			  ("both %<signed%> and %<void%> in "
			   "declaration specifiers"));
	      else if (specs->unsigned_p)
		error_at (loc,
			  ("both %<unsigned%> and %<void%> in "
			   "declaration specifiers"));
	      else if (specs->complex_p)
		error_at (loc,
			  ("both %<complex%> and %<void%> in "
			   "declaration specifiers"));
	      else if (specs->saturating_p)
		error_at (loc,
			  ("both %<_Sat%> and %<void%> in "
			   "declaration specifiers"));
	      else
		{
		  specs->typespec_word = cts_void;
		  specs->locations[cdw_typespec] = loc;
		}
	      return specs;
	    case RID_BOOL:
	      if (!in_system_header_at (loc))
		pedwarn_c90 (loc, OPT_Wpedantic,
			     "ISO C90 does not support boolean types");
	      if (specs->long_p)
		error_at (loc,
			  ("both %<long%> and %<_Bool%> in "
			   "declaration specifiers"));
	      else if (specs->short_p)
		error_at (loc,
			  ("both %<short%> and %<_Bool%> in "
			   "declaration specifiers"));
	      else if (specs->signed_p)
		error_at (loc,
			  ("both %<signed%> and %<_Bool%> in "
			   "declaration specifiers"));
	      else if (specs->unsigned_p)
		error_at (loc,
			  ("both %<unsigned%> and %<_Bool%> in "
			   "declaration specifiers"));
	      else if (specs->complex_p)
		error_at (loc,
			  ("both %<complex%> and %<_Bool%> in "
			   "declaration specifiers"));
	      else if (specs->saturating_p)
		error_at (loc,
			  ("both %<_Sat%> and %<_Bool%> in "
			   "declaration specifiers"));
	      else
		{
		  specs->typespec_word = cts_bool;
		  specs->locations[cdw_typespec] = loc;
		}
	      return specs;
	    case RID_CHAR:
	      if (specs->long_p)
		error_at (loc,
			  ("both %<long%> and %<char%> in "
			   "declaration specifiers"));
	      else if (specs->short_p)
		error_at (loc,
			  ("both %<short%> and %<char%> in "
			   "declaration specifiers"));
	      else if (specs->saturating_p)
		error_at (loc,
			  ("both %<_Sat%> and %<char%> in "
			   "declaration specifiers"));
	      else
		{
		  specs->typespec_word = cts_char;
		  specs->locations[cdw_typespec] = loc;
		}
	      return specs;
	    case RID_INT:
	      if (specs->saturating_p)
		error_at (loc,
			  ("both %<_Sat%> and %<int%> in "
			   "declaration specifiers"));
	      else
		{
		  specs->typespec_word = cts_int;
		  specs->locations[cdw_typespec] = loc;
		}
	      return specs;
	    case RID_FLOAT:
	      if (specs->long_p)
		error_at (loc,
			  ("both %<long%> and %<float%> in "
			   "declaration specifiers"));
	      else if (specs->short_p)
		error_at (loc,
			  ("both %<short%> and %<float%> in "
			   "declaration specifiers"));
	      else if (specs->signed_p)
		error_at (loc,
			  ("both %<signed%> and %<float%> in "
			   "declaration specifiers"));
	      else if (specs->unsigned_p)
		error_at (loc,
			  ("both %<unsigned%> and %<float%> in "
			   "declaration specifiers"));
	      else if (specs->saturating_p)
		error_at (loc,
			  ("both %<_Sat%> and %<float%> in "
			   "declaration specifiers"));
	      else
		{
		  specs->typespec_word = cts_float;
		  specs->locations[cdw_typespec] = loc;
		}
	      return specs;
	    case RID_DOUBLE:
	      if (specs->long_long_p)
		error_at (loc,
			  ("both %<long long%> and %<double%> in "
			   "declaration specifiers"));
	      else if (specs->short_p)
		error_at (loc,
			  ("both %<short%> and %<double%> in "
			   "declaration specifiers"));
	      else if (specs->signed_p)
		error_at (loc,
			  ("both %<signed%> and %<double%> in "
			   "declaration specifiers"));
	      else if (specs->unsigned_p)
		error_at (loc,
			  ("both %<unsigned%> and %<double%> in "
			   "declaration specifiers"));
	      else if (specs->saturating_p)
		error_at (loc,
			  ("both %<_Sat%> and %<double%> in "
			   "declaration specifiers"));
	      else
		{
		  specs->typespec_word = cts_double;
		  specs->locations[cdw_typespec] = loc;
		}
	      return specs;
	    CASE_RID_FLOATN_NX:
	      specs->floatn_nx_idx = i - RID_FLOATN_NX_FIRST;
	      if (!in_system_header_at (input_location))
		pedwarn (loc, OPT_Wpedantic,
			 "ISO C does not support the %<_Float%d%s%> type",
			 floatn_nx_types[specs->floatn_nx_idx].n,
			 (floatn_nx_types[specs->floatn_nx_idx].extended
			  ? "x"
			  : ""));

	      if (specs->long_p)
		error_at (loc,
			  ("both %<long%> and %<_Float%d%s%> in "
			   "declaration specifiers"),
			  floatn_nx_types[specs->floatn_nx_idx].n,
			  (floatn_nx_types[specs->floatn_nx_idx].extended
			   ? "x"
			   : ""));
	      else if (specs->short_p)
		error_at (loc,
			  ("both %<short%> and %<_Float%d%s%> in "
			   "declaration specifiers"),
			  floatn_nx_types[specs->floatn_nx_idx].n,
			  (floatn_nx_types[specs->floatn_nx_idx].extended
			   ? "x"
			   : ""));
	      else if (specs->signed_p)
		error_at (loc,
			  ("both %<signed%> and %<_Float%d%s%> in "
			   "declaration specifiers"),
			  floatn_nx_types[specs->floatn_nx_idx].n,
			  (floatn_nx_types[specs->floatn_nx_idx].extended
			   ? "x"
			   : ""));
	      else if (specs->unsigned_p)
		error_at (loc,
			  ("both %<unsigned%> and %<_Float%d%s%> in "
			   "declaration specifiers"),
			  floatn_nx_types[specs->floatn_nx_idx].n,
			  (floatn_nx_types[specs->floatn_nx_idx].extended
			   ? "x"
			   : ""));
	      else if (specs->saturating_p)
		error_at (loc,
			  ("both %<_Sat%> and %<_Float%d%s%> in "
			   "declaration specifiers"),
			  floatn_nx_types[specs->floatn_nx_idx].n,
			  (floatn_nx_types[specs->floatn_nx_idx].extended
			   ? "x"
			   : ""));
	      else if (FLOATN_NX_TYPE_NODE (specs->floatn_nx_idx) == NULL_TREE)
		{
		  specs->typespec_word = cts_floatn_nx;
		  error_at (loc,
			    "%<_Float%d%s%> is not supported on this target",
			    floatn_nx_types[specs->floatn_nx_idx].n,
			    (floatn_nx_types[specs->floatn_nx_idx].extended
			     ? "x"
			     : ""));
		}
	      else
		{
		  specs->typespec_word = cts_floatn_nx;
		  specs->locations[cdw_typespec] = loc;
		}
	      return specs;
	    case RID_DFLOAT32:
	    case RID_DFLOAT64:
	    case RID_DFLOAT128:
	      {
		const char *str;
		if (i == RID_DFLOAT32)
		  str = "_Decimal32";
		else if (i == RID_DFLOAT64)
		  str = "_Decimal64";
		else
		  str = "_Decimal128";
		if (specs->long_long_p)
		  error_at (loc,
			    ("both %<long long%> and %qs in "
			     "declaration specifiers"),
			    str);
		if (specs->long_p)
		  error_at (loc,
			    ("both %<long%> and %qs in "
			     "declaration specifiers"),
			    str);
		else if (specs->short_p)
		  error_at (loc,
			    ("both %<short%> and %qs in "
			     "declaration specifiers"),
			    str);
		else if (specs->signed_p)
		  error_at (loc,
			    ("both %<signed%> and %qs in "
			     "declaration specifiers"),
			    str);
		else if (specs->unsigned_p)
		  error_at (loc,
			    ("both %<unsigned%> and %qs in "
			     "declaration specifiers"),
			    str);
                else if (specs->complex_p)
                  error_at (loc,
			    ("both %<complex%> and %qs in "
			     "declaration specifiers"),
			    str);
                else if (specs->saturating_p)
                  error_at (loc,
			    ("both %<_Sat%> and %qs in "
			     "declaration specifiers"),
			    str);
		else if (i == RID_DFLOAT32)
		  specs->typespec_word = cts_dfloat32;
		else if (i == RID_DFLOAT64)
		  specs->typespec_word = cts_dfloat64;
		else
		  specs->typespec_word = cts_dfloat128;
		specs->locations[cdw_typespec] = loc;
	      }
	      if (!targetm.decimal_float_supported_p ())
		error_at (loc,
			  ("decimal floating point not supported "
			   "for this target"));
	      pedwarn (loc, OPT_Wpedantic,
		       "ISO C does not support decimal floating point");
	      return specs;
	    case RID_FRACT:
	    case RID_ACCUM:
	      {
		const char *str;
		if (i == RID_FRACT)
		  str = "_Fract";
		else
		  str = "_Accum";
                if (specs->complex_p)
                  error_at (loc,
			    ("both %<complex%> and %qs in "
			     "declaration specifiers"),
			    str);
		else if (i == RID_FRACT)
		    specs->typespec_word = cts_fract;
		else
		    specs->typespec_word = cts_accum;
		specs->locations[cdw_typespec] = loc;
	      }
	      if (!targetm.fixed_point_supported_p ())
		error_at (loc,
			  "fixed-point types not supported for this target");
	      pedwarn (loc, OPT_Wpedantic,
		       "ISO C does not support fixed-point types");
	      return specs;
	    default:
	      /* ObjC reserved word "id", handled below.  */
	      break;
	    }
	}
    }

  /* Now we have a typedef (a TYPE_DECL node), an identifier (some
     form of ObjC type, cases such as "int" and "long" being handled
     above), a TYPE (struct, union, enum and typeof specifiers) or an
     ERROR_MARK.  In none of these cases may there have previously
     been any type specifiers.  */
  if (specs->type || specs->typespec_word != cts_none
      || specs->long_p || specs->short_p || specs->signed_p
      || specs->unsigned_p || specs->complex_p)
    error_at (loc, "two or more data types in declaration specifiers");
  else if (TREE_CODE (type) == TYPE_DECL)
    {
      if (TREE_TYPE (type) == error_mark_node)
	; /* Allow the type to default to int to avoid cascading errors.  */
      else
	{
	  specs->type = TREE_TYPE (type);
	  specs->decl_attr = DECL_ATTRIBUTES (type);
	  specs->typedef_p = true;
	  specs->explicit_signed_p = C_TYPEDEF_EXPLICITLY_SIGNED (type);
	  specs->locations[cdw_typedef] = loc;

	  /* If this typedef name is defined in a struct, then a C++
	     lookup would return a different value.  */
	  if (warn_cxx_compat
	      && I_SYMBOL_BINDING (DECL_NAME (type))->in_struct)
	    warning_at (loc, OPT_Wc___compat,
			"C++ lookup of %qD would return a field, not a type",
			type);

	  /* If we are parsing a struct, record that a struct field
	     used a typedef.  */
	  if (warn_cxx_compat && struct_parse_info != NULL)
	    struct_parse_info->typedefs_seen.safe_push (type);
	}
    }
  else if (TREE_CODE (type) == IDENTIFIER_NODE)
    {
      tree t = lookup_name (type);
      if (!t || TREE_CODE (t) != TYPE_DECL)
	error_at (loc, "%qE fails to be a typedef or built in type", type);
      else if (TREE_TYPE (t) == error_mark_node)
	;
      else
	{
	  specs->type = TREE_TYPE (t);
	  specs->locations[cdw_typespec] = loc;
	}
    }
  else
    {
      if (TREE_CODE (type) != ERROR_MARK && spec.kind == ctsk_typeof)
	{
	  specs->typedef_p = true;
	  specs->locations[cdw_typedef] = loc;
	  if (spec.expr)
	    {
	      if (specs->expr)
		specs->expr = build2 (COMPOUND_EXPR, TREE_TYPE (spec.expr),
				      specs->expr, spec.expr);
	      else
		specs->expr = spec.expr;
	      specs->expr_const_operands &= spec.expr_const_operands;
	    }
	}
      specs->type = type;
    }

  return specs;
}

/* Add the storage class specifier or function specifier SCSPEC to the
   declaration specifiers SPECS, returning SPECS.  */

struct c_declspecs *
declspecs_add_scspec (source_location loc,
		      struct c_declspecs *specs,
		      tree scspec)
{
  enum rid i;
  enum c_storage_class n = csc_none;
  bool dupe = false;
  specs->declspecs_seen_p = true;
  gcc_assert (TREE_CODE (scspec) == IDENTIFIER_NODE
	      && C_IS_RESERVED_WORD (scspec));
  i = C_RID_CODE (scspec);
  if (specs->non_sc_seen_p)
    warning (OPT_Wold_style_declaration,
             "%qE is not at beginning of declaration", scspec);
  switch (i)
    {
    case RID_INLINE:
      /* C99 permits duplicate inline.  Although of doubtful utility,
	 it seems simplest to permit it in gnu89 mode as well, as
	 there is also little utility in maintaining this as a
	 difference between gnu89 and C99 inline.  */
      dupe = false;
      specs->inline_p = true;
      specs->locations[cdw_inline] = loc;
      break;
    case RID_NORETURN:
      /* Duplicate _Noreturn is permitted.  */
      dupe = false;
      specs->noreturn_p = true;
      specs->locations[cdw_noreturn] = loc;
      break;
    case RID_THREAD:
      dupe = specs->thread_p;
      if (specs->storage_class == csc_auto)
	error ("%qE used with %<auto%>", scspec);
      else if (specs->storage_class == csc_register)
	error ("%qE used with %<register%>", scspec);
      else if (specs->storage_class == csc_typedef)
	error ("%qE used with %<typedef%>", scspec);
      else
	{
	  specs->thread_p = true;
	  specs->thread_gnu_p = (strcmp (IDENTIFIER_POINTER (scspec),
					 "__thread") == 0);
	  /* A diagnostic is not required for the use of this
	     identifier in the implementation namespace; only diagnose
	     it for the C11 spelling because of existing code using
	     the other spelling.  */
	  if (!specs->thread_gnu_p)
	    {
	      if (flag_isoc99)
		pedwarn_c99 (loc, OPT_Wpedantic,
			     "ISO C99 does not support %qE", scspec);
	      else
		pedwarn_c99 (loc, OPT_Wpedantic,
			     "ISO C90 does not support %qE", scspec);
	    }
	  specs->locations[cdw_thread] = loc;
	}
      break;
    case RID_AUTO:
      n = csc_auto;
      break;
    case RID_EXTERN:
      n = csc_extern;
      /* Diagnose "__thread extern".  */
      if (specs->thread_p && specs->thread_gnu_p)
	error ("%<__thread%> before %<extern%>");
      break;
    case RID_REGISTER:
      n = csc_register;
      break;
    case RID_STATIC:
      n = csc_static;
      /* Diagnose "__thread static".  */
      if (specs->thread_p && specs->thread_gnu_p)
	error ("%<__thread%> before %<static%>");
      break;
    case RID_TYPEDEF:
      n = csc_typedef;
      break;
    default:
      gcc_unreachable ();
    }
  if (n != csc_none && n == specs->storage_class)
    dupe = true;
  if (dupe)
    {
      if (i == RID_THREAD)
	error ("duplicate %<_Thread_local%> or %<__thread%>");
      else
	error ("duplicate %qE", scspec);
    }
  if (n != csc_none)
    {
      if (specs->storage_class != csc_none && n != specs->storage_class)
	{
	  error ("multiple storage classes in declaration specifiers");
	}
      else
	{
	  specs->storage_class = n;
	  specs->locations[cdw_storage_class] = loc;
	  if (n != csc_extern && n != csc_static && specs->thread_p)
	    {
	      error ("%qs used with %qE",
		     specs->thread_gnu_p ? "__thread" : "_Thread_local",
		     scspec);
	      specs->thread_p = false;
	    }
	}
    }
  return specs;
}

/* Add the attributes ATTRS to the declaration specifiers SPECS,
   returning SPECS.  */

struct c_declspecs *
declspecs_add_attrs (source_location loc, struct c_declspecs *specs, tree attrs)
{
  specs->attrs = chainon (attrs, specs->attrs);
  specs->locations[cdw_attributes] = loc;
  specs->declspecs_seen_p = true;
  return specs;
}

/* Add an _Alignas specifier (expression ALIGN, or type whose
   alignment is ALIGN) to the declaration specifiers SPECS, returning
   SPECS.  */
struct c_declspecs *
declspecs_add_alignas (source_location loc,
		       struct c_declspecs *specs, tree align)
{
  int align_log;
  specs->alignas_p = true;
  specs->locations[cdw_alignas] = loc;
  if (align == error_mark_node)
    return specs;
  align_log = check_user_alignment (align, true);
  if (align_log > specs->align_log)
    specs->align_log = align_log;
  return specs;
}

/* Combine "long", "short", "signed", "unsigned" and "_Complex" type
   specifiers with any other type specifier to determine the resulting
   type.  This is where ISO C checks on complex types are made, since
   "_Complex long" is a prefix of the valid ISO C type "_Complex long
   double".  */

struct c_declspecs *
finish_declspecs (struct c_declspecs *specs)
{
  /* If a type was specified as a whole, we have no modifiers and are
     done.  */
  if (specs->type != NULL_TREE)
    {
      gcc_assert (!specs->long_p && !specs->long_long_p && !specs->short_p
		  && !specs->signed_p && !specs->unsigned_p
		  && !specs->complex_p);

      /* Set a dummy type.  */
      if (TREE_CODE (specs->type) == ERROR_MARK)
        specs->type = integer_type_node;
      return specs;
    }

  /* If none of "void", "_Bool", "char", "int", "float" or "double"
     has been specified, treat it as "int" unless "_Complex" is
     present and there are no other specifiers.  If we just have
     "_Complex", it is equivalent to "_Complex double", but e.g.
     "_Complex short" is equivalent to "_Complex short int".  */
  if (specs->typespec_word == cts_none)
    {
      if (specs->saturating_p)
	{
	  error_at (specs->locations[cdw_saturating],
		    "%<_Sat%> is used without %<_Fract%> or %<_Accum%>");
	  if (!targetm.fixed_point_supported_p ())
	    error_at (specs->locations[cdw_saturating],
		      "fixed-point types not supported for this target");
	  specs->typespec_word = cts_fract;
	}
      else if (specs->long_p || specs->short_p
	       || specs->signed_p || specs->unsigned_p)
	{
	  specs->typespec_word = cts_int;
	}
      else if (specs->complex_p)
	{
	  specs->typespec_word = cts_double;
	  pedwarn (specs->locations[cdw_complex], OPT_Wpedantic,
		   "ISO C does not support plain %<complex%> meaning "
		   "%<double complex%>");
	}
      else
	{
	  specs->typespec_word = cts_int;
	  specs->default_int_p = true;
	  /* We don't diagnose this here because grokdeclarator will
	     give more specific diagnostics according to whether it is
	     a function definition.  */
	}
    }

  /* If "signed" was specified, record this to distinguish "int" and
     "signed int" in the case of a bit-field with
     -funsigned-bitfields.  */
  specs->explicit_signed_p = specs->signed_p;

  /* Now compute the actual type.  */
  switch (specs->typespec_word)
    {
    case cts_auto_type:
      gcc_assert (!specs->long_p && !specs->short_p
		  && !specs->signed_p && !specs->unsigned_p
		  && !specs->complex_p);
      /* Type to be filled in later.  */
      break;
    case cts_void:
      gcc_assert (!specs->long_p && !specs->short_p
		  && !specs->signed_p && !specs->unsigned_p
		  && !specs->complex_p);
      specs->type = void_type_node;
      break;
    case cts_bool:
      gcc_assert (!specs->long_p && !specs->short_p
		  && !specs->signed_p && !specs->unsigned_p
		  && !specs->complex_p);
      specs->type = boolean_type_node;
      break;
    case cts_char:
      gcc_assert (!specs->long_p && !specs->short_p);
      gcc_assert (!(specs->signed_p && specs->unsigned_p));
      if (specs->signed_p)
	specs->type = signed_char_type_node;
      else if (specs->unsigned_p)
	specs->type = unsigned_char_type_node;
      else
	specs->type = char_type_node;
      if (specs->complex_p)
	{
	  pedwarn (specs->locations[cdw_complex], OPT_Wpedantic,
		   "ISO C does not support complex integer types");
	  specs->type = build_complex_type (specs->type);
	}
      break;
    case cts_int_n:
      gcc_assert (!specs->long_p && !specs->short_p && !specs->long_long_p);
      gcc_assert (!(specs->signed_p && specs->unsigned_p));
      if (! int_n_enabled_p[specs->int_n_idx])
	specs->type = integer_type_node;
      else
	specs->type = (specs->unsigned_p
		       ? int_n_trees[specs->int_n_idx].unsigned_type
		       : int_n_trees[specs->int_n_idx].signed_type);
      if (specs->complex_p)
	{
	  pedwarn (specs->locations[cdw_complex], OPT_Wpedantic,
		   "ISO C does not support complex integer types");
	  specs->type = build_complex_type (specs->type);
	}
      break;
    case cts_int:
      gcc_assert (!(specs->long_p && specs->short_p));
      gcc_assert (!(specs->signed_p && specs->unsigned_p));
      if (specs->long_long_p)
	specs->type = (specs->unsigned_p
		       ? long_long_unsigned_type_node
		       : long_long_integer_type_node);
      else if (specs->long_p)
	specs->type = (specs->unsigned_p
		       ? long_unsigned_type_node
		       : long_integer_type_node);
      else if (specs->short_p)
	specs->type = (specs->unsigned_p
		       ? short_unsigned_type_node
		       : short_integer_type_node);
      else
	specs->type = (specs->unsigned_p
		       ? unsigned_type_node
		       : integer_type_node);
      if (specs->complex_p)
	{
	  pedwarn (specs->locations[cdw_complex], OPT_Wpedantic,
		   "ISO C does not support complex integer types");
	  specs->type = build_complex_type (specs->type);
	}
      break;
    case cts_float:
      gcc_assert (!specs->long_p && !specs->short_p
		  && !specs->signed_p && !specs->unsigned_p);
      specs->type = (specs->complex_p
		     ? complex_float_type_node
		     : float_type_node);
      break;
    case cts_double:
      gcc_assert (!specs->long_long_p && !specs->short_p
		  && !specs->signed_p && !specs->unsigned_p);
      if (specs->long_p)
	{
	  specs->type = (specs->complex_p
			 ? complex_long_double_type_node
			 : long_double_type_node);
	}
      else
	{
	  specs->type = (specs->complex_p
			 ? complex_double_type_node
			 : double_type_node);
	}
      break;
    case cts_floatn_nx:
      gcc_assert (!specs->long_p && !specs->short_p
		  && !specs->signed_p && !specs->unsigned_p);
      if (FLOATN_NX_TYPE_NODE (specs->floatn_nx_idx) == NULL_TREE)
	specs->type = integer_type_node;
      else if (specs->complex_p)
	specs->type = COMPLEX_FLOATN_NX_TYPE_NODE (specs->floatn_nx_idx);
      else
	specs->type = FLOATN_NX_TYPE_NODE (specs->floatn_nx_idx);
      break;
    case cts_dfloat32:
    case cts_dfloat64:
    case cts_dfloat128:
      gcc_assert (!specs->long_p && !specs->long_long_p && !specs->short_p
		  && !specs->signed_p && !specs->unsigned_p && !specs->complex_p);
      if (specs->typespec_word == cts_dfloat32)
	specs->type = dfloat32_type_node;
      else if (specs->typespec_word == cts_dfloat64)
	specs->type = dfloat64_type_node;
      else
	specs->type = dfloat128_type_node;
      break;
    case cts_fract:
      gcc_assert (!specs->complex_p);
      if (!targetm.fixed_point_supported_p ())
	specs->type = integer_type_node;
      else if (specs->saturating_p)
	{
	  if (specs->long_long_p)
	    specs->type = specs->unsigned_p
			  ? sat_unsigned_long_long_fract_type_node
			  : sat_long_long_fract_type_node;
	  else if (specs->long_p)
	    specs->type = specs->unsigned_p
			  ? sat_unsigned_long_fract_type_node
			  : sat_long_fract_type_node;
	  else if (specs->short_p)
	    specs->type = specs->unsigned_p
			  ? sat_unsigned_short_fract_type_node
			  : sat_short_fract_type_node;
	  else
	    specs->type = specs->unsigned_p
			  ? sat_unsigned_fract_type_node
			  : sat_fract_type_node;
	}
      else
	{
	  if (specs->long_long_p)
	    specs->type = specs->unsigned_p
			  ? unsigned_long_long_fract_type_node
			  : long_long_fract_type_node;
	  else if (specs->long_p)
	    specs->type = specs->unsigned_p
			  ? unsigned_long_fract_type_node
			  : long_fract_type_node;
	  else if (specs->short_p)
	    specs->type = specs->unsigned_p
			  ? unsigned_short_fract_type_node
			  : short_fract_type_node;
	  else
	    specs->type = specs->unsigned_p
			  ? unsigned_fract_type_node
			  : fract_type_node;
	}
      break;
    case cts_accum:
      gcc_assert (!specs->complex_p);
      if (!targetm.fixed_point_supported_p ())
	specs->type = integer_type_node;
      else if (specs->saturating_p)
	{
	  if (specs->long_long_p)
	    specs->type = specs->unsigned_p
			  ? sat_unsigned_long_long_accum_type_node
			  : sat_long_long_accum_type_node;
	  else if (specs->long_p)
	    specs->type = specs->unsigned_p
			  ? sat_unsigned_long_accum_type_node
			  : sat_long_accum_type_node;
	  else if (specs->short_p)
	    specs->type = specs->unsigned_p
			  ? sat_unsigned_short_accum_type_node
			  : sat_short_accum_type_node;
	  else
	    specs->type = specs->unsigned_p
			  ? sat_unsigned_accum_type_node
			  : sat_accum_type_node;
	}
      else
	{
	  if (specs->long_long_p)
	    specs->type = specs->unsigned_p
			  ? unsigned_long_long_accum_type_node
			  : long_long_accum_type_node;
	  else if (specs->long_p)
	    specs->type = specs->unsigned_p
			  ? unsigned_long_accum_type_node
			  : long_accum_type_node;
	  else if (specs->short_p)
	    specs->type = specs->unsigned_p
			  ? unsigned_short_accum_type_node
			  : short_accum_type_node;
	  else
	    specs->type = specs->unsigned_p
			  ? unsigned_accum_type_node
			  : accum_type_node;
	}
      break;
    default:
      gcc_unreachable ();
    }

  return specs;
}

/* Perform final processing on one file scope's declarations (or the
   external scope's declarations), GLOBALS.  */

static void
c_write_global_declarations_1 (tree globals)
{
  tree decl;
  bool reconsider;

  /* Process the decls in the order they were written.  */
  for (decl = globals; decl; decl = DECL_CHAIN (decl))
    {
      /* Check for used but undefined static functions using the C
	 standard's definition of "used", and set TREE_NO_WARNING so
	 that check_global_declaration doesn't repeat the check.  */
      if (TREE_CODE (decl) == FUNCTION_DECL
	  && DECL_INITIAL (decl) == NULL_TREE
	  && DECL_EXTERNAL (decl)
	  && !TREE_PUBLIC (decl))
	{
	  if (C_DECL_USED (decl))
	    {
	      pedwarn (input_location, 0, "%q+F used but never defined", decl);
	      TREE_NO_WARNING (decl) = 1;
	    }
	  /* For -Wunused-function warn about unused static prototypes.  */
	  else if (warn_unused_function
		   && ! DECL_ARTIFICIAL (decl)
		   && ! TREE_NO_WARNING (decl))
	    {
	      warning (OPT_Wunused_function,
		       "%q+F declared %<static%> but never defined", decl);
	      TREE_NO_WARNING (decl) = 1;
	    }
	}

      wrapup_global_declaration_1 (decl);
    }

  do
    {
      reconsider = false;
      for (decl = globals; decl; decl = DECL_CHAIN (decl))
	reconsider |= wrapup_global_declaration_2 (decl);
    }
  while (reconsider);
}

/* Callback to collect a source_ref from a DECL.  */

static void
collect_source_ref_cb (tree decl)
{
  if (!DECL_IS_BUILTIN (decl))
    collect_source_ref (LOCATION_FILE (decl_sloc (decl, false)));
}

/* Preserve the external declarations scope across a garbage collect.  */
static GTY(()) tree ext_block;

/* Collect all references relevant to SOURCE_FILE.  */

static void
collect_all_refs (const char *source_file)
{
  tree t;
  unsigned i;

  FOR_EACH_VEC_ELT (*all_translation_units, i, t)
    collect_ada_nodes (BLOCK_VARS (DECL_INITIAL (t)), source_file);

  collect_ada_nodes (BLOCK_VARS (ext_block), source_file);
}

/* Iterate over all global declarations and call CALLBACK.  */

static void
for_each_global_decl (void (*callback) (tree decl))
{
  tree t;
  tree decls;
  tree decl;
  unsigned i;

  FOR_EACH_VEC_ELT (*all_translation_units, i, t)
    { 
      decls = DECL_INITIAL (t);
      for (decl = BLOCK_VARS (decls); decl; decl = TREE_CHAIN (decl))
	callback (decl);
    }

  for (decl = BLOCK_VARS (ext_block); decl; decl = TREE_CHAIN (decl))
    callback (decl);
}

/* Perform any final parser cleanups and generate initial debugging
   information.  */

void
c_parse_final_cleanups (void)
{
  tree t;
  unsigned i;

  /* We don't want to do this if generating a PCH.  */
  if (pch_file)
    return;

  timevar_stop (TV_PHASE_PARSING);
  timevar_start (TV_PHASE_DEFERRED);

  /* Do the Objective-C stuff.  This is where all the Objective-C
     module stuff gets generated (symtab, class/protocol/selector
     lists etc).  */
  if (c_dialect_objc ())
    objc_write_global_declarations ();

  /* Close the external scope.  */
  ext_block = pop_scope ();
  external_scope = 0;
  gcc_assert (!current_scope);

  /* Handle -fdump-ada-spec[-slim]. */
  if (flag_dump_ada_spec || flag_dump_ada_spec_slim)
    {
      /* Build a table of files to generate specs for */
      if (flag_dump_ada_spec_slim)
	collect_source_ref (main_input_filename);
      else
	for_each_global_decl (collect_source_ref_cb);

      dump_ada_specs (collect_all_refs, NULL);
    }

  /* Process all file scopes in this compilation, and the external_scope,
     through wrapup_global_declarations.  */
  FOR_EACH_VEC_ELT (*all_translation_units, i, t)
    c_write_global_declarations_1 (BLOCK_VARS (DECL_INITIAL (t)));
  c_write_global_declarations_1 (BLOCK_VARS (ext_block));

  timevar_stop (TV_PHASE_DEFERRED);
  timevar_start (TV_PHASE_PARSING);

  ext_block = NULL;
}

/* Register reserved keyword WORD as qualifier for address space AS.  */

void
c_register_addr_space (const char *word, addr_space_t as)
{
  int rid = RID_FIRST_ADDR_SPACE + as;
  tree id;

  /* Address space qualifiers are only supported
     in C with GNU extensions enabled.  */
  if (c_dialect_objc () || flag_no_asm)
    return;

  id = get_identifier (word);
  C_SET_RID_CODE (id, rid);
  C_IS_RESERVED_WORD (id) = 1;
  ridpointers [rid] = id;
}

/* Return identifier to look up for omp declare reduction.  */

tree
c_omp_reduction_id (enum tree_code reduction_code, tree reduction_id)
{
  const char *p = NULL;
  switch (reduction_code)
    {
    case PLUS_EXPR: p = "+"; break;
    case MULT_EXPR: p = "*"; break;
    case MINUS_EXPR: p = "-"; break;
    case BIT_AND_EXPR: p = "&"; break;
    case BIT_XOR_EXPR: p = "^"; break;
    case BIT_IOR_EXPR: p = "|"; break;
    case TRUTH_ANDIF_EXPR: p = "&&"; break;
    case TRUTH_ORIF_EXPR: p = "||"; break;
    case MIN_EXPR: p = "min"; break;
    case MAX_EXPR: p = "max"; break;
    default:
      break;
    }

  if (p == NULL)
    {
      if (TREE_CODE (reduction_id) != IDENTIFIER_NODE)
	return error_mark_node;
      p = IDENTIFIER_POINTER (reduction_id);
    }

  const char prefix[] = "omp declare reduction ";
  size_t lenp = sizeof (prefix);
  size_t len = strlen (p);
  char *name = XALLOCAVEC (char, lenp + len);
  memcpy (name, prefix, lenp - 1);
  memcpy (name + lenp - 1, p, len + 1);
  return get_identifier (name);
}

/* Lookup REDUCTION_ID in the current scope, or create an artificial
   VAR_DECL, bind it into the current scope and return it.  */

tree
c_omp_reduction_decl (tree reduction_id)
{
  struct c_binding *b = I_SYMBOL_BINDING (reduction_id);
  if (b != NULL && B_IN_CURRENT_SCOPE (b))
    return b->decl;

  tree decl = build_decl (BUILTINS_LOCATION, VAR_DECL,
			  reduction_id, integer_type_node);
  DECL_ARTIFICIAL (decl) = 1;
  DECL_EXTERNAL (decl) = 1;
  TREE_STATIC (decl) = 1;
  TREE_PUBLIC (decl) = 0;
  bind (reduction_id, decl, current_scope, true, false, BUILTINS_LOCATION);
  return decl;
}

/* Lookup REDUCTION_ID in the first scope where it has entry for TYPE.  */

tree
c_omp_reduction_lookup (tree reduction_id, tree type)
{
  struct c_binding *b = I_SYMBOL_BINDING (reduction_id);
  while (b)
    {
      tree t;
      for (t = DECL_INITIAL (b->decl); t; t = TREE_CHAIN (t))
	if (comptypes (TREE_PURPOSE (t), type))
	  return TREE_VALUE (t);
      b = b->shadowed;
    }
  return error_mark_node;
}

/* Helper function called via walk_tree, to diagnose invalid
   #pragma omp declare reduction combiners or initializers.  */

tree
c_check_omp_declare_reduction_r (tree *tp, int *, void *data)
{
  tree *vars = (tree *) data;
  if (SSA_VAR_P (*tp)
      && !DECL_ARTIFICIAL (*tp)
      && *tp != vars[0]
      && *tp != vars[1])
    {
      location_t loc = DECL_SOURCE_LOCATION (vars[0]);
      if (strcmp (IDENTIFIER_POINTER (DECL_NAME (vars[0])), "omp_out") == 0)
	error_at (loc, "%<#pragma omp declare reduction%> combiner refers to "
		       "variable %qD which is not %<omp_out%> nor %<omp_in%>",
		  *tp);
      else
	error_at (loc, "%<#pragma omp declare reduction%> initializer refers "
		       "to variable %qD which is not %<omp_priv%> nor "
		       "%<omp_orig%>",
		  *tp);
      return *tp;
    }
  return NULL_TREE;
}

#include "gt-c-c-decl.h"
