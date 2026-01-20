/* Type definitions for the ALGOL 68 parser.
   Copyright (C) 2001-2023 J. Marcel van der Veer.
   Copyright (C) 2025 Jose E. Marchesi.

   Original implementation by J. Marcel van der Veer.
   Adapted for GCC by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef __A68_TYPES_H__
#define __A68_TYPES_H__

#include "config.h"
#include "system.h"

#include <setjmp.h>
#include "vec.h"

/* Enumerations.  */

enum a68_stropping
{
  UPPER_STROPPING,
  SUPPER_STROPPING
};

enum a68_attribute
{
  STOP = 0,
#define A68_ATTR(ATTR,DESCR) ATTR,
#include "a68-parser-attrs.def"
#undef A68_ATTR
};

enum a68_tree_index
{
  /* Type trees.  */
  ATI_VOID_TYPE,
  ATI_BOOL_TYPE,
  ATI_CHAR_TYPE,
  ATI_SHORT_SHORT_BITS_TYPE,
  ATI_SHORT_BITS_TYPE,
  ATI_BITS_TYPE,
  ATI_LONG_BITS_TYPE,
  ATI_LONG_LONG_BITS_TYPE,
  ATI_BYTES_TYPE,
  ATI_LONG_BYTES_TYPE,
  ATI_SHORT_SHORT_INT_TYPE,
  ATI_SHORT_INT_TYPE,
  ATI_INT_TYPE,
  ATI_LONG_INT_TYPE,
  ATI_LONG_LONG_INT_TYPE,
  ATI_REAL_TYPE,
  ATI_LONG_REAL_TYPE,
  ATI_LONG_LONG_REAL_TYPE,
  /* Sentinel.  */
  ATI_MAX
};

/*
 * Type definitions.
 */

typedef char BUFFER[BUFFER_SIZE + 1];

struct MODES_T;
struct NODE_T;
struct MODE_CACHE_T;
struct MOID_T;
struct GINFO_T;
struct KEYWORD_T;
struct LINE_T;
struct NODE_INFO_T;
struct PACK_T;
struct SOID_T;
struct TABLE_T;
struct TAG_T;
struct TOKEN_T;
struct ORIGIN_T;
struct POSTULATE_T;
struct OPTIONS_T;
struct PARSER_T;
struct MODULE_T;
struct EXTRACT_T;
struct MOIF_T;
struct A68_T;

constexpr GINFO_T *NO_GINFO = nullptr;
constexpr KEYWORD_T *NO_KEYWORD = nullptr;
constexpr NODE_INFO_T *NO_NINFO = nullptr;
constexpr void (*NO_NOTE) (NODE_T *) = nullptr;
constexpr PACK_T *NO_PACK = nullptr;
constexpr SOID_T *NO_SOID = nullptr;
constexpr char *NO_TEXT = nullptr;
constexpr bool *NO_TICK = nullptr;
constexpr TOKEN_T *NO_TOKEN = nullptr;
#define NO_VAR nullptr

/* A STATUS_MASK_T is a word of flags denoting states.

   Status masks are used in parse tree nodes (NODE_T) and in entries in the
   symbol table (TAG_T).


   SCOPE_ERROR_MASK is used by the static scope checker in order to avoid
   emitting duplicated scope warnings.  */

typedef uint32_t STATUS_MASK_T;

#define STATUS_CLEAR(p, q) {STATUS (p) &= (~(q));}
#define STATUS_SET(p, q) {STATUS (p) |= (q);}
#define STATUS_TEST(p, q) ((STATUS (p) & (q)) != (uint32_t) 0)

#define NULL_MASK                 ((STATUS_MASK_T) 0x00000000)
#define SCOPE_ERROR_MASK          ((STATUS_MASK_T) 0x00000200)

/* Structure containing the lowering context which is propagated while calling
   the lowering handlers.  */

struct LOW_CTX_T
{
  /* The common declarer in a declaration list.  */
  NODE_T **declarer;
  /* The defining identifier of the procedure declaration being processed, or
     NO_NODE.  This is set by a68_lower_procedure_declaration and used by
     a68_lower_routine_text.  */
  NODE_T *proc_decl_identifier;
  /* If proc_decl_identifier is no NO_NODE, this denotes whether the
     declaration being processed is of an operator.  */
  bool proc_decl_operator;
  /* Name of current module definition, or NULL.  */
  const char *module_definition_name;
  /* For debugging purposes.  */
  int level;
};

typedef struct LOW_CTX_T LOW_CTX_T;

/* Type of the lowerer routines defined in a68-low-prelude.cc.  */
typedef tree (*LOWERER_T) (struct NODE_T *, struct LOW_CTX_T);

#define NO_LOWERER a68_lower_unimplemented

struct GTY((chain_next ("%h.more"), chain_prev ("%h.less"))) KEYWORD_T
{
  enum a68_attribute attribute;
  const char *text;
  KEYWORD_T *less, *more;
};

/* A MOID_T represents a mode indicator.

   NUMBER is an unique number assigned to the moid when it gets created.  A
   renumber_moids function exists in a68-parser-modes.cc but it dosn't seem to
   be used at all.

   ATTRIBUTE characterizes the kind of mode and is one of the values defined in
   a68-parser-attrs.def.  Valid values are:

     PROC_SYMBOL for procecure modes.
     ROWS_SYMBOL for row modes.
     REF_SYMBOL for reference modes.
     FLEX_SYMBOL for flexible reference modes.
     STRUCT_SYMBOL for struct modes.
     UNION_SYMBOL for union modes.
     IN_TYPE_MODE for XXX modes.
     OUT_TYPE_MODE for XXX modes.
     SERIES_MODE for XXX modes.
     INDICANT for XXX modes.
     MODULE_INDICANT for XXX modules.
     STANDARD for standard modes.

   NODE is a parse tree node XXX.

   HAS_ROWS is true if the mode contains rows somewhere in its internal
   structure.

   HAS_REFS is true if the mode contains refs somewhere in its internal
   structure.

   The interpretation of SUB depends on the kind of mode:
   - For REF modes it is the referred mode.
   - For FLEX modes it is the referred mode.
   - For ROW modes it is the mode of the elements.
   - For PROC modes it is the mode of the value yielded by the procedure.

   The interpretation of DIM depends on the kind of mode:
   - In VOID_SYMBOL, STANDARD or INDICANT mode, if DIM is positive it
     specifies the size of the longsety of the mode.  If DIM is negative then
     abs (DIM) is the size of hte shortsety of the mode.
   - In ROW modes, DIM is the number of dimensions.
   - In STRUCT modes, DIM is the number of fields.
   - In UNION modes, DIM is the number of united modes.
   - In PROC_SYMBOL modes, DIM is the number of arguments.
   - In SERIES_MODE and STOWED_MODE modes, DIM is the number modes.

   SLICE is the mode resulting from slicing a value of this mode.  For example,
   slicing a M_ROW_INT yields a M_INT.

   EQUIVALENT_MODE (referred as EQUIVALENT), for INDICANTs it is its declarer
   mode (in MODE FOO = BAR FOO is the indicant and BAR is its declarer) and for
   STANDARD modes it may be a mode that reflects its structure.

   USE is used by the is_well_formed function, which detects whether a mode is
   well formed, i.e. that the mode doesn't refer to itself nor it relates to
   void.

   DEFLEXED_MODE is like the current mode, but without FLEX.  Only defined for
   modes that have a SUB, i.e. REF, FLEX, ROW and PROC.  In other modes this is
   NO_MODE.

   For refs to structs, rows or flex, NAME points to the corresponding name
   mode.  For example, for a mode REF STRUCT (INT i, REAL x), NAME points to a
   mode STRUCT (REF INT i, REF REAL x).  This is used for selections.

   For rows of structs, rows or flex, MULTIPLE points to the corresponding row
   mode.  For example, for a mode [] STRUCT (INT i, REAL x), MULTIPLE points to
   a mode STRUCT ([]INT i, []REAL x).  This is used for selections.

   PACK is a pack of moids.  For a STOWED_MODE, it contains the modes of the
   arguments of a procedure, or the modes of the units in a collateral clause.
   For a SERIES_MODE, it contains the modes of the completing units in a serial
   clause, the alternatives in a conformity clause, the alternatives in a
   CONDITIONAL_CLAUSE, the alternatives in a CASE_CLAUSE,

   CTYPE is a GCC GENERIC tree corresponding to this mode.  It is computed and
   installed by a68_lower_moid.

   ASM_LABEL is an assembly label used by a68_asm_output_mode.  */

#define NO_MOID ((MOID_T *) 0)

struct GTY((chain_next ("%h.next"))) MOID_T
{
  int number;
  int attribute;
  int dim;
  bool has_rows, has_refs, use, portable, derivate;
  NODE_T *node;
  PACK_T *pack;
  MOID_T *sub, *equivalent_mode, *slice, *deflexed_mode, *name, *multiple_mode, *next, *rowed, *trim;
  tree ctype;
  const char *asm_label;
};

/* A MODES_T struct contains a collection of particular pre-defined modes.

   These modes are initialized by either stand_moids or make_special_mode.
   They are commonly referred using the corresponding M_* macros.

   ROWS is a mode to which any ROW mode can be strongly coerced.  It is used as
   the mode of the second operand of the ELEMS, LWB and UPB operators.

   HIP is the mode of NIL.  */

struct MODES_T
{
  MOID_T *BITS, *BOOL, *BYTES, *CHANNEL, *CHAR, *COLLITEM, *COMPL, *COMPLEX,
    *C_STRING, *ERROR, *FILE, *FORMAT, *HEX_NUMBER, *HIP, *INT, *LONG_BITS, *LONG_BYTES,
    *LONG_COMPL, *LONG_COMPLEX, *LONG_INT, *LONG_LONG_BITS, *LONG_LONG_COMPL,
    *LONG_LONG_COMPLEX, *LONG_LONG_INT, *LONG_LONG_REAL, *LONG_REAL, *NUMBER,
    *PROC_REAL_REAL, *PROC_LONG_REAL_LONG_REAL, *PROC_REF_FILE_BOOL, *PROC_REF_FILE_VOID, *PROC_ROW_CHAR,
    *PROC_STRING, *PROC_VOID, *REAL, *REF_BITS, *REF_BOOL, *REF_BYTES,
    *REF_CHAR, *REF_COMPL, *REF_COMPLEX, *REF_FILE, *REF_INT,
    *REF_LONG_BITS, *REF_LONG_BYTES, *REF_LONG_COMPL, *REF_LONG_COMPLEX,
    *REF_LONG_INT, *REF_LONG_LONG_BITS, *REF_LONG_LONG_COMPL,
    *REF_LONG_LONG_COMPLEX, *REF_LONG_LONG_INT, *REF_LONG_LONG_REAL, *REF_LONG_REAL,
    *REF_REAL, *REF_REF_FILE, *REF_ROW_CHAR, *REF_ROW_COMPLEX, *REF_ROW_INT,
    *REF_ROW_REAL, *REF_ROW_ROW_COMPLEX, *REF_ROW_ROW_REAL,
    *REF_SHORT_BITS, *REF_SHORT_SHORT_BITS, *REF_SHORT_INT,
    *REF_SHORT_SHORT_INT, *REF_STRING,
    *ROW_BITS, *ROW_BOOL, *ROW_CHAR, *ROW_COMPLEX, *ROW_INT, *ROW_LONG_BITS, *ROW_LONG_LONG_BITS,
    *ROW_REAL, *ROW_ROW_CHAR, *ROW_ROW_COMPLEX, *ROW_ROW_REAL, *ROWS, *ROW_SIMPLIN, *ROW_SIMPLOUT,
    *ROW_STRING, *SEMA, *SHORT_BITS, *SHORT_SHORT_BITS, *SHORT_INT, *SHORT_SHORT_INT,
    *SIMPLIN, *SIMPLOUT, *STRING, *FLEX_ROW_CHAR,
    *FLEX_ROW_BOOL, *UNDEFINED, *VACUUM, *VOID;
};

/* The OPTIONS_T structure record which front-end options have been activated.
   Each option OPTION has a corresponding GCC -f[no-]a68-OPTION command-line
   switch that can be used to activate or deactivate it.

   STROPPING indicates the stropping regime in use.  It can be UPPER_STROPPING
   or SUPPER_STROPPING.

   BRACKETS indicates whether [ .. ] and { .. } are equivalent to ( .. ).

   STRICT indicates that no ALGOL 68 extension is allowed.

   ASSERT indicates whether to generate code for assertions.

   BOUNDS_CHECKING indicates whether to perform array bound checking at
   run-time.

   NIL_CHECKING indicates whether to check for NIL when dereferencing at
   run-time.  */

struct GTY(()) OPTIONS_T
{
  enum a68_stropping stropping;
  bool brackets;
  bool strict;
  bool assert;
  bool bounds_checking;
  bool nil_checking;
};

/* The access class static property of a stored value determines how the value
   can be reached at run-time.  It is used by the lowering pass in order to
   minimize copies at run-time.

   CONSTANT is for constant literals.  At run-time these literals will either
   reside in operand instructions or in space allocated in CONSTAB%.

   DIRIDEN (direct identifier) means that the value is stored on IDST% at some
   static address.  This is the access class used for values ascribed to
   identifiers as long as the block in hich they are declared has not been
   left.  It is also used for values resulting from actions such as the
   selection from a value possessed by an identifier or the dereferencing fo a
   name corespodning to a variable.

   VARIDEN (variable identifier) is used for values which are names/variables.
   The name is stored on IDST%.  The static elaboration of the dereferencing of
   a variable with access VARIDEN results in a value with access DIRIDEN, not
   requiring any run-time action.  Same happens with selections of variables of
   access VARIDEN.

   INDIDEN (indirect identifier) is used for values that are stored in a memory
   location in IDST%.  The static elaboration of a dereferencing applied to a
   value of access DIRIDEN.

   DIRWOST (direct working stack) is very much like DIRIDEN, except that the
   value is stored in WOST% rathern than in IDST%.  This access is used for the
   result of an action when this result does not preexist in memory and hence
   has to be constructed in WOST%.

   INDWOST (indirect working stack) is very similar to INDIDEN.  Such an access
   can be obtained for example through the static elaboration of the
   dereferencing of a name the access of which is DIRWOST.

   NIHIL is used to characterize the absence of value.  This is used in the
   static elaboration of a jump, a voiding and a call ith a void result.

   Note that in all these classes we assume as run-time the intermediate
   language level we are lowering to, i.e. GENERIC.  A DIRIDEN value, for
   example, can very well stored in a register depending on further compiler
   optimizations.  */

#define ACCESS_NIHIL 0
#define ACCESS_CONSTANT 1
#define ACCESS_DIRIDEN 2
#define ACCESS_INDIDEN 3
#define ACCESS_DIRWOST 4

/* A NODE_T is a node in the A68 Syntax tree produced by the lexer-scanner and
   later expanded by the Mailloux parser.

   NUMBER identifies the node uniquely in the syntax tree.

   ATTRIBUTE is a code that specifies the kind of entity denoted by the node.
   Valid attributes are defined in the enumeration a68_attribute above in
   this file.  Examples of attributes are ELIF_PART, GOTO_SYMBOL or BOLD_TAG.

   ANNOTATION provides a way to annotate a node with a reference to another
   node attribute.  This is currently used by the mode checker to annotate
   indexer nodes as slicer or as trimmers.

   TYPE (accessed as MOID) is either NO_MOID if the entity denoted by the node
   doesn't have a mode, or a suitable MOID_T reflecting the mode of the entity.
   This attribute is calculated and set in all the nodes of the tree by the
   mode collection and checker pass implemented by make_moid_list.

   INFO contains additional attributes of the node.  See NODE_INFO_T below.

   NEXT, PREVIOUS and SUB are links to other tree nodes.  They are used to link
   the syntax tree structure from the top:

        TOP <-> N <-> N <-> ...
                |                      < is PREVIOUS
		N <-> N <-> ...        > is NEXT
                      |                | is SUB
                      N <-> ...

   SEQUENCE is a link to another tree node.  It is used by the tax collector
   (symbol table builder) in order to handle DO .. OD ranges.

   NEST is a link to another tree node, which is the NEST for the current node.
   It is set by the tax collector (symbol table builder) and it is used for
   diagnostics.

   PACK (accessed as NODE_PACK) is either NO_PACK or an instance of the PACK_T
   structure defined below.  It is used by the modes checker.

   STATUS is a mask of flags, used by several passes that handle nodes.  Valid status flags are:

   SYMBOL_TABLE (accessed as TABLE) is either NO_TABLE or a TABLE_T containing
   a symbol table introduced by the entity denoted by the tree node.  These
   nodes are the ones introducing ranges: BEGIN, DO, etc.

   NON_LOCAL is either NO_TABLE, if the environ established by the node is
   local, or a pointer to a TABLE_T identifying the non-local environment
   associated with the tree node.  It is set by the static scope checker.  See
   3.2.2 and 5.2.3.2.b in the report for its application in the handling of
   local generators in serial clauses.

   TAG (accessed as TAX) is either NO_TAG or a TAG_T used to bind identifier
   nodes, routine texts and other indicants to their corresponding entry in a
   symbol table.  This is set by the taxes collector.

   The following fields are static properties managed an used in the lowering
   pass.

   ORIGIN is a static property that describes the history of the entity denoted
   by the node.  This is only used in nodes denoting values.

   DYNAMIC_STACK_ALLOCS is a flag used in serial clause nodes.  It determines
   whether the elaboration of the phrases in the serial clause may involve
   dynamic stack allocation.  This is used by the lower pass, along with
   NON_LOCAL above, in order to properly manage the stack pointer while
   lowering these clauses.

   PUBLICIZED is true for DEFINING_OPERATOR, DEFINING_IDENTIFIER and
   DEFINING_INDICANT nodes that appear in declarations marked with PUB.

   CDECL is a GCC GENERIC tree corresponding to a DECL_FIELD for FIELD
   nodes.  */

struct GTY((chain_next ("%h.next"), chain_prev ("%h.previous"))) NODE_T
{
  GINFO_T *genie;
  int number;
  enum a68_attribute attribute;
  enum a68_attribute annotation;
  MOID_T *type;
  NODE_INFO_T *info;
  NODE_T *next, *previous, *sub;
  NODE_T *sequence, *nest;
  PACK_T *pack;
  STATUS_MASK_T status;
  TABLE_T *symbol_table;
  TABLE_T *non_local;
  TAG_T *tag;
  tree cdecl;
  bool dynamic_stack_allocs;
  bool publicized;
};

#define NO_NODE ((NODE_T *) 0)

/* A NODE_INFO_T struct contains additional attributes of a NODE_T parse tree
   node.

   PROCEDURE_LEVEL indicates how lexically deep the tree node is in terms of
   routine texts.  This attribute is set for all the nodes in the syntax tree
   by the taxes collector and originally used by ALGOL 68 Genie's monitor.
   Even if at the moment this is not used by GCC, this field and the
   correponding machinery is still here in case it is useful in the future.

   PRIORITY (accessed as PRIO) is used by operator tree nodes and specifies the
   priority of the operator denoted by the node.  This is set tree wide by the
   bottom-up parser.

   SYMBOL (accessed indirectly as NSYMBOL) contains the symbol value, a string,
   corresponding to the tree node.  This is used by tree nodes representing
   tokens such as bold tags, keywords and identifiers.  The symbols are set by
   the parser-scanner.

   LINE is a pointer to the source line from which the tree node originates.
   This is set for tree nodes representing tokens and is set by the
   parser-scanner.  */

struct GTY(()) NODE_INFO_T
{
  int procedure_level;
  int priority;
  char * GTY((skip)) char_in_line;
  int comment_type;
  char * GTY((skip)) comment;
  LINE_T *comment_line;
  char * GTY((skip)) comment_char_in_line;
  int pragmat_type;
  char * GTY((skip)) pragmat;
  LINE_T *pragmat_line;
  char * GTY((skip)) pragmat_char_in_line;
  const char *symbol;
  LINE_T *line;
};

struct GTY(()) GINFO_T
{
  MOID_T *partial_proc, *partial_locale;
};

struct GTY((chain_next ("%h.next"), chain_prev ("%h.previous"))) PACK_T
{
  MOID_T *type;
  const char *text;
  NODE_T *node;
  PACK_T *next, *previous;
};

/* Postulates.  */

struct GTY((chain_next ("%h.next"))) POSTULATE_T
{
  MOID_T *a, *b;
  POSTULATE_T *next;
};

#define NO_POSTULATE ((POSTULATE_T *) 0)

struct GTY((chain_next ("%h.next"))) SOID_T
{
  int attribute, sort, cast;
  MOID_T *type;
  NODE_T *node;
  SOID_T *next;
};

struct GTY((chain_next ("%h.next"))) LINE_T
{
  char marker[6];
  char * GTY((skip)) string;
  const char *filename;
  int number;
  LINE_T *next, *previous;
};
#define NO_LINE ((LINE_T *) 0)

/* Symbol table.

   PUBLIC_RANGE is true in ranges whose declarations may be made accessible to
   other compilation units.  decl trees lowered for declarations in public
   ranges will be put in the top-level block.  This is used for top-level
   module declarations.  */

struct GTY(()) TABLE_T
{
  int num, level, nest, attribute;
  bool initialise_frame, initialise_anon, proc_ops, public_range;
  TABLE_T *previous, *outer;
  TAG_T *identifiers, *operators, *modules, *priority, *indicants, *labels, *anonymous;
  NODE_T *jump_to, *sequence;
};
#define NO_TABLE ((TABLE_T *) 0)

/* A TAG_T structure denotes an entry in the symbol table.  Each entry
   corresponds to an identity.

   TAX: TAG; TAB; TAD; TAM;

   TYPE is the mode of the entry.

   NODE is the defining identifier associated to the declaration.

   SCOPE is the lexical depth of the tag.  Zero corresponds to the primal
   scope.  It is set by the static scope checker.

   SCOPE_ASSIGNED determines whether a SCOPE has been actually assigned to the
   tag.  It is set by the static scope checker.  The entities which get
   assigned scopes are identities of format texts and routine texts.

   PORTABLE determines whether the construction associated with the tag is
   Algol 68 or some extension.

   VARIABLE is set when the defining identifier in NODE is defined in a
   variable declaration, as opposed to an identity declaration.  This is set by
   extract_variables and is used by the lowering pass.

   HEAP is used for defining identifier in NODE is defined in a variable
   declaration.  It is HEAP_SYMBOL or LOC_SYMBOL.

   IS_RECURSIVE is set for mode indicants whose definition is recursive,
   i.e. they appear in actual declarers within its own definition.

   PUBLICIZED is set for tags that are marked as public and therefore shall be
   exported as part of a module interface.

   ASCRIBED_ROUTINE_TEXT is set when the defining identifier is ascribed a
   routine-text in an identity declaration.

   IN_PROC is set when the defining identifier has been set in a
   proc-identity-declaration or in a brief-op-declaration.  These declarations
   are optimized in a similar way than variable declarations in order to avoid
   indirect addressing.

   YOUNGEST_ENVIRON is used when NODE is either a ROUTINE_TEXT or a
   FORMAT_TEXT, and contains the youngest (higher) lexical level of any object
   directly declared in the routine or format body.  This is filled in and used
   by the scope checker.

   TREE_DECL is the GENERIC declaration for the definition of this symbol.
   This is set and used by the lower pass.  For mode indicants, it contains a
   function that generates a pointer to the given mode, and is used by
   a68_low_generator to handle recursive modes.

   MOIF is a list of module interfaces.  This is used in ACCESS_CLAUSE nodes.

   EXTERN_SYMBOL is a string with the symbol that was obtained from an imported
   module declaration.  This is only used in entries where MOIF is not NO_MOIF.

   LOWERER is a lowering routine defined in a68-low-prelude.cc.  These are used
   in taxes that denote some pre-defined operator.  */

struct GTY((chain_next ("%h.next"))) TAG_T
{
  TABLE_T *symbol_table;
  MOID_T *type;
  NODE_T *node, *unit;
  const char *value;
  bool scope_assigned, use, in_proc, loc_assigned, portable, variable;
  bool ascribed_routine_text, is_recursive, publicized;
  int priority, heap, scope, youngest_environ, number;
  STATUS_MASK_T status;
  tree tree_decl;
  MOIF_T *moif;
  LOWERER_T lowerer;
  TAG_T *next, *body;
  const char *extern_symbol;
};
#define NO_TAG ((TAG_T *) 0)

struct GTY((chain_next ("%h.more"), chain_prev ("%h.less"))) TOKEN_T
{
  const char *text;
  TOKEN_T *less, *more;
};
#define NO_TOKEN ((TOKEN_T *) 0)

struct GTY(()) MODULE_T
{
  bool tree_listing_safe, cross_reference_safe;
  int error_count, warning_count, source_scan;
  LINE_T *top_line;
  MOID_T *top_moid, *standenv_moid;
  MOIF_T *top_moif;
  NODE_T *top_node;
  OPTIONS_T options;
  FILE * GTY ((skip)) file_source_fd;
  const char *file_source_name;
  struct
  {
    LINE_T *save_l;
    char * GTY((skip)) save_s, GTY((skip)) save_c;
  } scan_state;
};

/* Module interface extracts.

   KIND is the kind of extract.  One of the GA68_EXTRACT_* codes defined in
   a68-exports.cc.

   SYMBOL is a string with the symbol name for the entity represented by the
   extract.  It is mangled.

   MODE applies to identifier, operator and indicatione extracts.

   PRIORITY is the priority number in a priority extract.

   VARIABLE applies to GA68_EXTRACT_IDEN and GA68_EXTRACT_OPER extracts, and
   indicates whether the exported symbol was declared via a variable
   declaration. These decls are optimized and don't require indirect
   addressing.  This is compiler-specific and part of mdextra.

   IN_PROC applies to GA68_EXTRACT_IDEN and GA68_EXTRACT_OPER extracts.  If
   set, the exported symbol shall not be indirected.  This is compiler-specific
   and part of mdextra.  */

struct GTY(()) EXTRACT_T
{
  unsigned int kind;
  const char *symbol;
  MOID_T *mode;
  int priority;
  bool variable, in_proc;
};

/* Module interfaces.

   VERSION is the version of the exports format to report in the encoded data.

   NAME is the name of the module as it is accessed at the source level, which
   corresponds to a bold word.

   PRELUDE and POSTLUDE are mangled symbols corresponding to the entry points
   of the module's prelude and postlude.

   MODES is a vector of modes which conform the modes table of the module
   interface.

   MODULES is a vector of TAGs for module extracts.
   INDICANTS is a vector of TAGs for mode extracts.
   IDENTIFIERS is a vector of TAGs for identifier extracts.
   PRIOS is a vector of TAGs for operator priorities.  */

#define NO_MOIF ((MOIF_T *) 0)

struct GTY((chain_next ("%h.next"))) MOIF_T
{
  unsigned int version;
  const char *name;
  const char *prelude;
  const char *postlude;
  vec<MOID_T*,va_gc> *modes;
  vec<EXTRACT_T*,va_gc> *modules;
  vec<EXTRACT_T*,va_gc> *indicants;
  vec<EXTRACT_T*,va_gc> *identifiers;
  vec<EXTRACT_T*,va_gc> *prios;
  vec<EXTRACT_T*,va_gc> *operators;
  MOIF_T *next;
};

struct MODE_CACHE_T
{
  MOID_T *proc_bool;
  MOID_T *proc_char;
  MOID_T *proc_complex_complex;
  MOID_T *proc_int;
  MOID_T *proc_int_int;
  MOID_T *proc_int_int_real;
  MOID_T *proc_int_real;
  MOID_T *proc_int_real_real;
  MOID_T *proc_int_real_real_real;
  MOID_T *proc_real;
  MOID_T *proc_real_int_real;
  MOID_T *proc_real_real;
  MOID_T *proc_real_real_int_real;
  MOID_T *proc_real_real_real;
  MOID_T *proc_real_real_real_int;
  MOID_T *proc_real_real_real_real;
  MOID_T *proc_real_real_real_real_real;
  MOID_T *proc_real_real_real_real_real_real;
  MOID_T *proc_real_ref_real_ref_int_void;
  MOID_T *proc_void;
};

/* A PARSER_T struct contains all the global state that is kept by the parser.
   This information is managed and used exclusively by a68_parser.

   ERROR_TAG is a tag that signifies an error.  It is initialized in
   a68_parser.

   STOP_SCANNER is a control flag used exclusively by the main loop in
   tokenise_source, which is recursive.

   SCAN_BUF is a scratch buffer used by the scanner for several purposes.  This
   buffer is known to be big enough to hold any substring from the source file.
   It is initialized in read_source_file.

   MAX_SCAN_BUF_LENGTH is the useable size of SCAN_BUF.  This is used by the
   scanner to grow SCAN_BUF as it includes other files.

   TAG_NUMBER is a global counter used by the parser to assign an unique number
   to each tag it creates.  It is used in a68_new_tag.

   BOTTOM_UP_CRASH_EXIT and TOP_DOWN_CRASH_EXIT are used to longjmp from deeply
   nested errors in the bottom-up and top-down parsers respectively.  */

struct GTY(()) PARSER_T
{
  TAG_T *error_tag;
  bool stop_scanner;
  size_t max_scan_buf_length;
  char * GTY((skip)) scan_buf;
  int tag_number;
  jmp_buf GTY((skip)) bottom_up_crash_exit;
  jmp_buf GTY((skip)) top_down_crash_exit;
};

/* A A68_T struct contains the global state used by the ALGOL 68 front-end.

   OUTPUT_LINE is used by the diagnostics machinery in order to write out
   message lines.

   EDIT_LINE is used as a scratch buffer for composing error messages and the
   like.

   INPUT_LINE is used by the original ALGOL 68 Genie to read lines from the
   tty.  But in this parser it is used uninitialized (!) by the
   a68_phrase_to_text routine in the top-down parser.  XXX.

   NEW_NODES is a global counter that keeps the number of parse tree nodes
   created.  It is currently not used for anything, but still updated in
   a68_new_node.

   NEW_MODES is a global counter that keeps the number of moids created.  It is
   currently not used for anything, but still updated in a68_new_moid.

   NEW_POSTULATES is a global counter that keeps the number of postulates
   created by the front-end.  It is currently not used for anything, but still
   updated in a68_make_postulate.

   NEW_NODE_INFOS is a global counter that keeps the number of NODE_INFO_T
   structures created by the front-end.  It is currently not used for anything,
   but still updated in a68_new_node_info.

   NEW_GENIE_INFOS is a global counter that keeps the number of GINFO_T
   structures created by the front-end.  It is currently no tused for anything,
   but still updated in a68_new_genie_info.

   SYMBOL_TABLE_COUNT is a global counter used by the parser.  XXX move to
   parser global state when syntax tree finalisation is moved to a68_parser?

   MODE_COUNT is the number of modes registered in the global modes table. XXX
   which table.

   TOP_KEYWORD is the top of the list of keywords known to the font-end.

   MODE_CACHE XXX.

   A68_MODES (accessed as MODE) is a collection of particular pre-defined
   modes.  These modes are initialized by stand_moids.

   JOB (accessed as A68_JOB) is the instance of MODULE_T with the global data
   corresponding to the source file being compiled.

   OPTIONS is the set of options currently set for the front-end.

   TOP_POSTULATE and TOP_POSTULATE_LIST are lists of postulates maintained by
   the front-end.

   POSTULATES is a collection of postulates used by the moid pretty printer.

   TOP_SOID_LIST is used by the moid machinery.

   STANDENV XXX.

   TOP_TOKEN XXX.

   INCLUDE_PATHS is the list of paths where we search for files to include.
   Directories are added to the list at the option handling language hook.
   The list is searched in FIFO order.

   IMPORT_PATHS is the list of paths where we search for module exports data.
   Directories are added to the list at the option handling language hook.  The
   list is searched in FIFO order.

   GLOBAL_TREES is an array with global types like a68_void_type and
   a68_long_int_type.  It is indexed by an enum a68_tree_index.

   MODULE_DEFINITION_DECLS is the global list of the tree decls corresponding
   to the module definition being compiled.  Note that currently we only allow
   top-level modules, so there is no nesting.  This is part of the context of
   the lowering pass, and these declarations are collected by the lowering
   handlers and finally compiled down to assembler in lower_module_declaration.
   It cannot go in LOW_CTX_T because the later is on the stack and is not
   reachable by the GGC.

   PARSER_STATE contains the parser's global state.

   GLOBAL_CONTEXT is the context to be used for global declarations.  Normally
   the translation unit.

   GLOBAL_DECLARATIONS contains a array of global declarations to pass back to
   the middle-end at the end of the compilation.
*/

struct GTY(()) A68_T
{
  BUFFER output_line;
  BUFFER edit_line;
  BUFFER input_line;
  int new_nodes;
  int new_modes;
  int new_postulates;
  int new_node_infos;
  int new_genie_infos;
  int symbol_table_count;
  int mode_count;
  KEYWORD_T *top_keyword;
  MODE_CACHE_T mode_cache;
  MODES_T a68_modes;
  MODULE_T job;
  OPTIONS_T *options;
  POSTULATE_T *postulates, *top_postulate, *top_postulate_list;
  SOID_T *top_soid_list;
  TABLE_T *standenv;
  TOKEN_T *top_token;
  vec<const char *, va_gc, vl_embed> *include_paths;
  vec<const char *, va_gc, vl_embed> *import_paths;
  hash_map<nofree_string_hash,const char *> *module_files;
  tree global_trees[ATI_MAX];
  PARSER_T parser_state;
  vec<tree,va_gc> *module_definition_decls;
  tree global_context;
  vec<tree, va_gc> *global_declarations;
};

/*
 * Access macros to fields in the struct types defined above.  These are used *
 * in order to achieve a nice ALGOL-like field OF struct style.
 */

#define ASM_LABEL(m) ((m)->asm_label)
#define BACKWARD(p) (p = PREVIOUS (p))
#define DEFLEX(p) (DEFLEXED (p) != NO_MOID ? DEFLEXED(p) : (p))
#define FORWARD(p) ((p) = NEXT (p))
#define A(p) ((p)->a)
#define ANNOTATION(p) ((p)->annotation)
#define ANONYMOUS(p) ((p)->anonymous)
#define ATTRIBUTE(p) ((p)->attribute)
#define ASCRIBED_ROUTINE_TEXT(p) ((p)->ascribed_routine_text)
#define B(p) ((p)->b)
#define BODY(p) ((p)->body)
#define CAST(p) ((p)->cast)
#define CHAR_IN_LINE(p) ((p)->char_in_line)
#define CROSS_REFERENCE_SAFE(p) ((p)->cross_reference_safe)
#define CDECL(p) ((p)->cdecl)
#define COMMENT(p) ((p)->comment)
#define COMMENT_CHAR_IN_LINE(p) ((p)->comment_char_in_line)
#define COMMENT_LINE(p) ((p)->comment_line)
#define COMMENT_TYPE(p) ((p)->comment_type)
#define CTYPE(p) ((p)->ctype)
#define DEFLEXED(p) ((p)->deflexed_mode)
#define DEREFO(p) ((p).derefo)
#define DERIVATE(p) ((p)->derivate)
#define DIM(p) ((p)->dim)
#define DYNAMIC_STACK_ALLOCS(p) ((p)->dynamic_stack_allocs)
#define EQUIVALENT(p) ((p)->equivalent_mode)
#define EQUIVALENT_MODE(p) ((p)->equivalent_mode)
#define ERROR_COUNT(p) ((p)->error_count)
#define EXTERN_SYMBOL(p) ((p)->extern_symbol)
#define EXTRACT_IN_PROC(p) ((p)->in_proc)
#define EXTRACT_KIND(p) ((p)->kind)
#define EXTRACT_MODE(p) ((p)->mode)
#define EXTRACT_PRIO(p) ((p)->priority)
#define EXTRACT_SYMBOL(p) ((p)->symbol)
#define EXTRACT_VARIABLE(p) ((p)->variable)
#define WARNING_COUNT(p) ((p)->warning_count)
#define F(p) ((p)->f)
#define FILENAME(p) ((p)->filename)
#define FILE_SOURCE_FD(p) ((p)->file_source_fd)
#define FILE_SOURCE_NAME(p) ((p)->file_source_name)
#define FLEXO(p) ((p).flexo)
#define FLEXO_KNOWN(p) ((p).flexo_known)
#define G(p) ((p)->g)
#define GINFO(p) ((p)->genie)
#define GENO(p) ((p).geno)
#define GET(p) ((p)->get)
#define GPARENT(p) (PARENT (GINFO (p)))
#define GREEN(p) ((p)->green)
#define H(p) ((p)->h)
#define HANDLE(p) ((p)->handle)
#define HAS_REFS(p) ((p)->has_refs)
#define HAS_ROWS(p) ((p)->has_rows)
#define HEAP(p) ((p)->heap)
#define ID(p) ((p)->id)
#define IDENTIFICATION(p) ((p)->identification)
#define IDENTIFIERS(p) ((p)->identifiers)
#define IDF(p) ((p)->idf)
#define IM(z) (VALUE (&(z)[1]))
#define IN(p) ((p)->in)
#define INDEX(p) ((p)->index)
#define INDICANTS(p) ((p)->indicants)
#define INFO(p) ((p)->info)
#define INITIALISE_ANON(p) ((p)->initialise_anon)
#define INITIALISE_FRAME(p) ((p)->initialise_frame)
#define INI_PTR(p) ((p)->ini_ptr)
#define INS_MODE(p) ((p)->ins_mode)
#define IN_FORBIDDEN(p) ((p)->in_forbidden)
#define IN_PREFIX(p) ((p)->in_prefix)
#define IN_PROC(p) ((p)->in_proc)
#define IN_TEXT(p) ((p)->in_text)
#define IS_OPEN(p) ((p)->is_open)
#define IS_RECURSIVE(p) ((p)->is_recursive)
#define IS_TMP(p) ((p)->is_tmp)
#define JUMP_STAT(p) ((p)->jump_stat)
#define JUMP_TO(p) ((p)->jump_to)
#define K(q) ((q)->k)
#define LABELS(p) ((p)->labels)
#define LAST(p) ((p)->last)
#define LAST_LINE(p) ((p)->last_line)
#define LESS(p) ((p)->less)
#define LEVEL(p) ((p)->level)
#define LEX_LEVEL(p) (LEVEL (TABLE (p)))
#define LINBUF(p) ((p)->linbuf)
#define LINE(p) ((p)->line)
#define LINE_APPLIED(p) ((p)->line_applied)
#define LINE_DEFINED(p) ((p)->line_defined)
#define LINE_END_MENDED(p) ((p)->line_end_mended)
#define LINE_NUMBER(p) (NUMBER (LINE (INFO (p))))
#define LINSIZ(p) ((p)->linsiz)
#define LIST(p) ((p)->list)
#define ln(x) (log (x))
#define LOCALE(p) ((p)->locale)
#define LOC_ASSIGNED(p) ((p)->loc_assigned)
#define LOWERER(p) ((p)->lowerer)
#define LOWER_BOUND(p) ((p)->lower_bound)
#define LWB(p) ((p)->lower_bound)
#define MARKER(p) ((p)->marker)
#define MATCH(p) ((p)->match)
#define MODIFIED(p) ((p)->modified)
#define MODULES(p) ((p)->modules)
#define VERSION(p) ((p)->version)
#define MODES(p) ((p)->modes)
#define MOID(p) ((p)->type)
#define MOIF(p) ((p)->moif)
#define MORE(p) ((p)->more)
#define MSGS(p) ((p)->msgs)
#define MULTIPLE(p) ((p)->multiple_mode)
#define MULTIPLE_MODE(p) ((p)->multiple_mode)
#define NAME(p) ((p)->name)
#define NEST(p) ((p)->nest)
#define NEXT(p) ((p)->next)
#define NEXT_NEXT(p) (NEXT (NEXT (p)))
#define NEXT_NEXT_NEXT(p) (NEXT (NEXT_NEXT (p)))
#define NEXT_SUB(p) (NEXT (SUB (p)))
#define NF(p) ((p)->nf)
#define NILO(p) ((p).nilo)
#define NILO_KNOWN(p) ((p).nilo_known)
#define NODE(p) ((p)->node)
#define NODE_DEFINED(p) ((p)->node_defined)
#define NODE_PACK(p) ((p)->pack)
#define NON_LOCAL(p) ((p)->non_local)
#define NCHAR_IN_LINE(p) (CHAR_IN_LINE (INFO (p)))
#define NCOMMENT(p) (COMMENT (INFO (p)))
#define NCOMMENT_CHAR_IN_LINE(p) (COMMENT_CHAR_IN_LINE (INFO (p)))
#define NCOMMENT_LINE(p) (COMMENT_LINE (INFO (p)))
#define NCOMMENT_TYPE(p) (COMMENT_TYPE (INFO (p)))
#define NPRAGMAT(p) (PRAGMAT (INFO (p)))
#define NPRAGMAT_CHAR_IN_LINE(p) (PRAGMAT_CHAR_IN_LINE (INFO (p)))
#define NPRAGMAT_LINE(p) (PRAGMAT_LINE (INFO (p)))
#define NPRAGMAT_TYPE(p) (PRAGMAT_TYPE (INFO (p)))
#define NSYMBOL(p) (SYMBOL (INFO (p)))
#define NUM(p) ((p)->num)
#define NUMBER(p) ((p)->number)
#define OPER(p) ((p)->oper)
#define OPERATORS(p) ((p)->operators)
#define OPT(p) ((p)->opt)
#define OPTIONS(p) ((p)->options)
#define OPTION_ASSERT(p) (OPTIONS (p).assert)
#define OPTION_BOUNDS_CHECKING(p) (OPTIONS (p).bounds_checking)
#define OPTION_BRACKETS(p) (OPTIONS (p).brackets)
#define OPTION_NIL_CHECKING(p) (OPTIONS (p).nil_checking)
#define OPTION_STRICT(p) (OPTIONS (p).strict)
#define OPTION_STROPPING(p) (OPTIONS (p).stropping)
#define OPTION_LIST(p) (OPTIONS (p).list)
#define OPTION_LOCAL(p) (OPTIONS (p).local)
#define OPTION_NODEMASK(p) (OPTIONS (p).nodemask)
#define OUT(p) ((p)->out)
#define OUTER(p) ((p)->outer)
#define P(q) ((q)->p)
#define PACK(p) ((p)->pack)
#define PARTIAL_LOCALE(p) ((p)->partial_locale)
#define PARTIAL_PROC(p) ((p)->partial_proc)
#define PORTABLE(p) ((p)->portable)
#define POSTLUDE(p) ((p)->postlude)
#define PRAGMAT(p) ((p)->pragmat)
#define PRAGMAT_CHAR_IN_LINE(p) ((p)->pragmat_char_in_line)
#define PRAGMAT_LINE(p) ((p)->pragmat_line)
#define PRAGMAT_TYPE(p) ((p)->pragmat_type)
#define PRELUDE(p) ((p)->prelude)
#define PREVIOUS(p) ((p)->previous)
#define PRIO(p) ((p)->priority)
#define PRIOS(p) ((p)->prios)
#define PROCEDURE_LEVEL(p) ((p)->procedure_level)
#define PROC_OPS(p) ((p)->proc_ops)
#define PUBLICIZED(p) ((p)->publicized)
#define PUBLIC_RANGE(p) ((p)->public_range)
#define R(p) ((p)->r)
#define RE(z) (VALUE (&(z)[0]))
#define ROWED(p) ((p)->rowed)
#define SCAN_STATE_C(p) ((p)->scan_state.save_c)
#define SCAN_STATE_L(p) ((p)->scan_state.save_l)
#define SCAN_STATE_S(p) ((p)->scan_state.save_s)
#define SCOPE(p) ((p)->scope)
#define SCOPE_ASSIGNED(p) ((p)->scope_assigned)
#define SEQUENCE(p) ((p)->sequence)
#define SEVERITY(p) ((p)->severity)
#define SLICE(p) ((p)->slice)
#define SORT(p) ((p)->sort)
#define SOURCE_SCAN(p) ((p)->source_scan)
#define STANDENV_MOID(p) ((p)->standenv_moid)
#define STATUS(p) ((p)->status)
#define STRING(p) ((p)->string)
#define SUB(p) ((p)->sub)
#define SUB_MOID(p) (SUB (MOID (p)))
#define SUB_NEXT(p) (SUB (NEXT (p)))
#define SUB_SUB(p) (SUB (SUB (p)))
#define SYMBOL(p) ((p)->symbol)
#define TABLE(p) ((p)->symbol_table)
#define TAG_LEX_LEVEL(p) (LEVEL (TAG_TABLE (p)))
#define TAG_TABLE(p) ((p)->symbol_table)
#define TAX(p) ((p)->tag)
#define TAX_TREE_DECL(p) ((p)->tree_decl)
#define TEXT(p) ((p)->text)
#define TOP_LINE(p) ((p)->top_line)
#define TOP_MOID(p) ((p)->top_moid)
#define TOP_MOIF(p) ((p)->top_moif)
#define TOP_NODE(p) ((p)->top_node)
#define TRANSIENT(p) ((p)->transient)
#define TREE_LISTING_SAFE(p) ((p)->tree_listing_safe)
#define TRIM(p) ((p)->trim)
#define UNIT(p) ((p)->unit)
#define USE(p) ((p)->use)
#define VALUE(p) ((p)->value)
#define VARIABLE(p) ((p)->variable)
#define WHERE(p) ((p)->where)
#define IS_FLEXETY_ROW(m) (IS_FLEX (m) || IS_ROW (m) || m == M_STRING)
#define IS_FLEX(m) IS ((m), FLEX_SYMBOL)
#define IS_LITERALLY(p, s) (strcmp (NSYMBOL (p), s) == 0)
#define ISNT(p, s) (! IS (p, s))
#define IS(p, s) (ATTRIBUTE (p) == (s))
#define IS_REF_FLEX(m) (IS (m, REF_SYMBOL) && IS (SUB (m), FLEX_SYMBOL))
#define IS_REF(m) IS ((m), REF_SYMBOL)
#define IS_INTEGRAL(m)					  \
  ((m) == M_INT						  \
   || (m) == M_LONG_INT					  \
   || (m) == M_LONG_LONG_INT				  \
   || (m) == M_SHORT_INT				  \
   || (m) == M_SHORT_SHORT_INT)
#define IS_BITS(m)						  \
  ((m) == M_BITS						  \
   || (m) == M_LONG_BITS					  \
   || (m) == M_LONG_LONG_BITS					  \
   || (m) == M_SHORT_BITS					  \
   || (m) == M_SHORT_SHORT_BITS)
#define IS_BYTES(m)				\
  ((m) == M_BYTES || (m) == M_LONG_BYTES)
#define IS_COMPLEX(m)				\
  ((m) == M_COMPLEX				\
   || (m) == M_LONG_COMPLEX			\
   || (m) == M_LONG_LONG_COMPLEX)
#define IS_REAL(m)				\
  ((m) == M_REAL				\
   || (m) == M_LONG_REAL			\
   || (m) == M_LONG_LONG_REAL)
#define IS_ROW(m) IS ((m), ROW_SYMBOL)
#define IS_STRUCT(m) IS ((m), STRUCT_SYMBOL)
#define IS_UNION(m) IS ((m), UNION_SYMBOL)
#define YOUNGEST_ENVIRON(p) ((p)->youngest_environ)

#endif /* ! __A68_TYPES_H */
