/* gfortran header file
   Copyright (C) 2000-2018 Free Software Foundation, Inc.
   Contributed by Andy Vaught

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

#ifndef GCC_GFORTRAN_H
#define GCC_GFORTRAN_H

/* It's probably insane to have this large of a header file, but it
   seemed like everything had to be recompiled anyway when a change
   was made to a header file, and there were ordering issues with
   multiple header files.  Besides, Microsoft's winnt.h was 250k last
   time I looked, so by comparison this is perfectly reasonable.  */

#ifndef GCC_CORETYPES_H
#error "gfortran.h must be included after coretypes.h"
#endif

/* In order for the format checking to accept the Fortran front end
   diagnostic framework extensions, you must include this file before
   diagnostic-core.h, not after.  We override the definition of GCC_DIAG_STYLE
   in c-common.h.  */
#undef GCC_DIAG_STYLE
#define GCC_DIAG_STYLE __gcc_gfc__
#if defined(GCC_DIAGNOSTIC_CORE_H)
#error \
In order for the format checking to accept the Fortran front end diagnostic \
framework extensions, you must include this file before diagnostic-core.h, \
not after.
#endif

/* Declarations common to the front-end and library are put in
   libgfortran/libgfortran_frontend.h  */
#include "libgfortran.h"


#include "intl.h"
#include "splay-tree.h"

/* Major control parameters.  */

#define GFC_MAX_SYMBOL_LEN 63   /* Must be at least 63 for F2003.  */
#define GFC_LETTERS 26		/* Number of letters in the alphabet.  */

#define MAX_SUBRECORD_LENGTH 2147483639   /* 2**31-9 */


#define gfc_is_whitespace(c) ((c==' ') || (c=='\t') || (c=='\f'))

/* Macros to check for groups of structure-like types and flavors since
   derived types, structures, maps, unions are often treated similarly. */
#define gfc_bt_struct(t) \
  ((t) == BT_DERIVED || (t) == BT_UNION)
#define gfc_fl_struct(f) \
  ((f) == FL_DERIVED || (f) == FL_UNION || (f) == FL_STRUCT)
#define case_bt_struct case BT_DERIVED: case BT_UNION
#define case_fl_struct case FL_DERIVED: case FL_UNION: case FL_STRUCT

/* Stringization.  */
#define stringize(x) expand_macro(x)
#define expand_macro(x) # x

/* For the runtime library, a standard prefix is a requirement to
   avoid cluttering the namespace with things nobody asked for.  It's
   ugly to look at and a pain to type when you add the prefix by hand,
   so we hide it behind a macro.  */
#define PREFIX(x) "_gfortran_" x
#define PREFIX_LEN 10

/* A prefix for internal variables, which are not user-visible.  */
#if !defined (NO_DOT_IN_LABEL)
# define GFC_PREFIX(x) "_F." x
#elif !defined (NO_DOLLAR_IN_LABEL)
# define GFC_PREFIX(x) "_F$" x
#else
# define GFC_PREFIX(x) "_F_" x
#endif

#define BLANK_COMMON_NAME "__BLNK__"

/* Macro to initialize an mstring structure.  */
#define minit(s, t) { s, NULL, t }

/* Structure for storing strings to be matched by gfc_match_string.  */
typedef struct
{
  const char *string;
  const char *mp;
  int tag;
}
mstring;



/*************************** Enums *****************************/

/* Used when matching and resolving data I/O transfer statements.  */

enum io_kind
{ M_READ, M_WRITE, M_PRINT, M_INQUIRE };


/* These are flags for identifying whether we are reading a character literal
   between quotes or normal source code.  */

enum gfc_instring
{ NONSTRING = 0, INSTRING_WARN, INSTRING_NOWARN };

/* This is returned by gfc_notification_std to know if, given the flags
   that were given (-std=, -pedantic) we should issue an error, a warning
   or nothing.  */

enum notification
{ SILENT, WARNING, ERROR };

/* Matchers return one of these three values.  The difference between
   MATCH_NO and MATCH_ERROR is that MATCH_ERROR means that a match was
   successful, but that something non-syntactic is wrong and an error
   has already been issued.  */

enum match
{ MATCH_NO = 1, MATCH_YES, MATCH_ERROR };

/* Used for different Fortran source forms in places like scanner.c.  */
enum gfc_source_form
{ FORM_FREE, FORM_FIXED, FORM_UNKNOWN };

/* Expression node types.  */
enum expr_t
{ EXPR_OP = 1, EXPR_FUNCTION, EXPR_CONSTANT, EXPR_VARIABLE,
  EXPR_SUBSTRING, EXPR_STRUCTURE, EXPR_ARRAY, EXPR_NULL, EXPR_COMPCALL, EXPR_PPC
};

/* Array types.  */
enum array_type
{ AS_EXPLICIT = 1, AS_ASSUMED_SHAPE, AS_DEFERRED,
  AS_ASSUMED_SIZE, AS_IMPLIED_SHAPE, AS_ASSUMED_RANK,
  AS_UNKNOWN
};

enum ar_type
{ AR_FULL = 1, AR_ELEMENT, AR_SECTION, AR_UNKNOWN };

/* Statement label types. ST_LABEL_DO_TARGET is used for obsolescent warnings
   related to shared DO terminations and DO targets which are neither END DO
   nor CONTINUE; otherwise it is identical to ST_LABEL_TARGET.  */
enum gfc_sl_type
{ ST_LABEL_UNKNOWN = 1, ST_LABEL_TARGET, ST_LABEL_DO_TARGET,
  ST_LABEL_BAD_TARGET, ST_LABEL_FORMAT
};

/* Intrinsic operators.  */
enum gfc_intrinsic_op
{ GFC_INTRINSIC_BEGIN = 0,
  INTRINSIC_NONE = -1, INTRINSIC_UPLUS = GFC_INTRINSIC_BEGIN,
  INTRINSIC_UMINUS, INTRINSIC_PLUS, INTRINSIC_MINUS, INTRINSIC_TIMES,
  INTRINSIC_DIVIDE, INTRINSIC_POWER, INTRINSIC_CONCAT,
  INTRINSIC_AND, INTRINSIC_OR, INTRINSIC_EQV, INTRINSIC_NEQV,
  /* ==, /=, >, >=, <, <=  */
  INTRINSIC_EQ, INTRINSIC_NE, INTRINSIC_GT, INTRINSIC_GE,
  INTRINSIC_LT, INTRINSIC_LE,
  /* .EQ., .NE., .GT., .GE., .LT., .LE. (OS = Old-Style)  */
  INTRINSIC_EQ_OS, INTRINSIC_NE_OS, INTRINSIC_GT_OS, INTRINSIC_GE_OS,
  INTRINSIC_LT_OS, INTRINSIC_LE_OS,
  INTRINSIC_NOT, INTRINSIC_USER, INTRINSIC_ASSIGN, INTRINSIC_PARENTHESES,
  GFC_INTRINSIC_END, /* Sentinel */
  /* User defined derived type pseudo operators. These are set beyond the
     sentinel so that they are excluded from module_read and module_write.  */
  INTRINSIC_FORMATTED, INTRINSIC_UNFORMATTED
};

/* This macro is the number of intrinsic operators that exist.
   Assumptions are made about the numbering of the interface_op enums.  */
#define GFC_INTRINSIC_OPS GFC_INTRINSIC_END

/* Arithmetic results.  */
enum arith
{ ARITH_OK = 1, ARITH_OVERFLOW, ARITH_UNDERFLOW, ARITH_NAN,
  ARITH_DIV0, ARITH_INCOMMENSURATE, ARITH_ASYMMETRIC, ARITH_PROHIBIT
};

/* Statements.  */
enum gfc_statement
{
  ST_ARITHMETIC_IF, ST_ALLOCATE, ST_ATTR_DECL, ST_ASSOCIATE,
  ST_BACKSPACE, ST_BLOCK, ST_BLOCK_DATA,
  ST_CALL, ST_CASE, ST_CLOSE, ST_COMMON, ST_CONTINUE, ST_CONTAINS, ST_CYCLE,
  ST_DATA, ST_DATA_DECL, ST_DEALLOCATE, ST_DO, ST_ELSE, ST_ELSEIF,
  ST_ELSEWHERE, ST_END_ASSOCIATE, ST_END_BLOCK, ST_END_BLOCK_DATA,
  ST_ENDDO, ST_IMPLIED_ENDDO, ST_END_FILE, ST_FINAL, ST_FLUSH, ST_END_FORALL,
  ST_END_FUNCTION, ST_ENDIF, ST_END_INTERFACE, ST_END_MODULE, ST_END_SUBMODULE,
  ST_END_PROGRAM, ST_END_SELECT, ST_END_SUBROUTINE, ST_END_WHERE, ST_END_TYPE,
  ST_ENTRY, ST_EQUIVALENCE, ST_ERROR_STOP, ST_EXIT, ST_FORALL, ST_FORALL_BLOCK,
  ST_FORMAT, ST_FUNCTION, ST_GOTO, ST_IF_BLOCK, ST_IMPLICIT, ST_IMPLICIT_NONE,
  ST_IMPORT, ST_INQUIRE, ST_INTERFACE, ST_SYNC_ALL, ST_SYNC_MEMORY,
  ST_SYNC_IMAGES, ST_PARAMETER, ST_MODULE, ST_SUBMODULE, ST_MODULE_PROC,
  ST_NAMELIST, ST_NULLIFY, ST_OPEN, ST_PAUSE, ST_PRIVATE, ST_PROGRAM, ST_PUBLIC,
  ST_READ, ST_RETURN, ST_REWIND, ST_STOP, ST_SUBROUTINE, ST_TYPE, ST_USE,
  ST_WHERE_BLOCK, ST_WHERE, ST_WAIT, ST_WRITE, ST_ASSIGNMENT,
  ST_POINTER_ASSIGNMENT, ST_SELECT_CASE, ST_SEQUENCE, ST_SIMPLE_IF,
  ST_STATEMENT_FUNCTION, ST_DERIVED_DECL, ST_LABEL_ASSIGNMENT, ST_ENUM,
  ST_ENUMERATOR, ST_END_ENUM, ST_SELECT_TYPE, ST_TYPE_IS, ST_CLASS_IS,
  ST_STRUCTURE_DECL, ST_END_STRUCTURE,
  ST_UNION, ST_END_UNION, ST_MAP, ST_END_MAP,
  ST_OACC_PARALLEL_LOOP, ST_OACC_END_PARALLEL_LOOP, ST_OACC_PARALLEL,
  ST_OACC_END_PARALLEL, ST_OACC_KERNELS, ST_OACC_END_KERNELS, ST_OACC_DATA,
  ST_OACC_END_DATA, ST_OACC_HOST_DATA, ST_OACC_END_HOST_DATA, ST_OACC_LOOP,
  ST_OACC_END_LOOP, ST_OACC_DECLARE, ST_OACC_UPDATE, ST_OACC_WAIT,
  ST_OACC_CACHE, ST_OACC_KERNELS_LOOP, ST_OACC_END_KERNELS_LOOP,
  ST_OACC_ENTER_DATA, ST_OACC_EXIT_DATA, ST_OACC_ROUTINE,
  ST_OACC_ATOMIC, ST_OACC_END_ATOMIC,
  ST_OMP_ATOMIC, ST_OMP_BARRIER, ST_OMP_CRITICAL, ST_OMP_END_ATOMIC,
  ST_OMP_END_CRITICAL, ST_OMP_END_DO, ST_OMP_END_MASTER, ST_OMP_END_ORDERED,
  ST_OMP_END_PARALLEL, ST_OMP_END_PARALLEL_DO, ST_OMP_END_PARALLEL_SECTIONS,
  ST_OMP_END_PARALLEL_WORKSHARE, ST_OMP_END_SECTIONS, ST_OMP_END_SINGLE,
  ST_OMP_END_WORKSHARE, ST_OMP_DO, ST_OMP_FLUSH, ST_OMP_MASTER, ST_OMP_ORDERED,
  ST_OMP_PARALLEL, ST_OMP_PARALLEL_DO, ST_OMP_PARALLEL_SECTIONS,
  ST_OMP_PARALLEL_WORKSHARE, ST_OMP_SECTIONS, ST_OMP_SECTION, ST_OMP_SINGLE,
  ST_OMP_THREADPRIVATE, ST_OMP_WORKSHARE, ST_OMP_TASK, ST_OMP_END_TASK,
  ST_OMP_TASKWAIT, ST_OMP_TASKYIELD, ST_OMP_CANCEL, ST_OMP_CANCELLATION_POINT,
  ST_OMP_TASKGROUP, ST_OMP_END_TASKGROUP, ST_OMP_SIMD, ST_OMP_END_SIMD,
  ST_OMP_DO_SIMD, ST_OMP_END_DO_SIMD, ST_OMP_PARALLEL_DO_SIMD,
  ST_OMP_END_PARALLEL_DO_SIMD, ST_OMP_DECLARE_SIMD, ST_OMP_DECLARE_REDUCTION,
  ST_OMP_TARGET, ST_OMP_END_TARGET, ST_OMP_TARGET_DATA, ST_OMP_END_TARGET_DATA,
  ST_OMP_TARGET_UPDATE, ST_OMP_DECLARE_TARGET,
  ST_OMP_TEAMS, ST_OMP_END_TEAMS, ST_OMP_DISTRIBUTE, ST_OMP_END_DISTRIBUTE,
  ST_OMP_DISTRIBUTE_SIMD, ST_OMP_END_DISTRIBUTE_SIMD,
  ST_OMP_DISTRIBUTE_PARALLEL_DO, ST_OMP_END_DISTRIBUTE_PARALLEL_DO,
  ST_OMP_DISTRIBUTE_PARALLEL_DO_SIMD, ST_OMP_END_DISTRIBUTE_PARALLEL_DO_SIMD,
  ST_OMP_TARGET_TEAMS, ST_OMP_END_TARGET_TEAMS, ST_OMP_TEAMS_DISTRIBUTE,
  ST_OMP_END_TEAMS_DISTRIBUTE, ST_OMP_TEAMS_DISTRIBUTE_SIMD,
  ST_OMP_END_TEAMS_DISTRIBUTE_SIMD, ST_OMP_TARGET_TEAMS_DISTRIBUTE,
  ST_OMP_END_TARGET_TEAMS_DISTRIBUTE, ST_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD,
  ST_OMP_END_TARGET_TEAMS_DISTRIBUTE_SIMD, ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO,
  ST_OMP_END_TEAMS_DISTRIBUTE_PARALLEL_DO,
  ST_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO,
  ST_OMP_END_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO,
  ST_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD,
  ST_OMP_END_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD,
  ST_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD,
  ST_OMP_END_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD,
  ST_OMP_TARGET_PARALLEL, ST_OMP_END_TARGET_PARALLEL,
  ST_OMP_TARGET_PARALLEL_DO, ST_OMP_END_TARGET_PARALLEL_DO,
  ST_OMP_TARGET_PARALLEL_DO_SIMD, ST_OMP_END_TARGET_PARALLEL_DO_SIMD,
  ST_OMP_TARGET_ENTER_DATA, ST_OMP_TARGET_EXIT_DATA,
  ST_OMP_TARGET_SIMD, ST_OMP_END_TARGET_SIMD,
  ST_OMP_TASKLOOP, ST_OMP_END_TASKLOOP,
  ST_OMP_TASKLOOP_SIMD, ST_OMP_END_TASKLOOP_SIMD, ST_OMP_ORDERED_DEPEND,
  ST_PROCEDURE, ST_GENERIC, ST_CRITICAL, ST_END_CRITICAL,
  ST_GET_FCN_CHARACTERISTICS, ST_LOCK, ST_UNLOCK, ST_EVENT_POST,
  ST_EVENT_WAIT, ST_FAIL_IMAGE, ST_FORM_TEAM, ST_CHANGE_TEAM,
  ST_END_TEAM, ST_SYNC_TEAM, ST_NONE
};

/* Types of interfaces that we can have.  Assignment interfaces are
   considered to be intrinsic operators.  */
enum interface_type
{
  INTERFACE_NAMELESS = 1, INTERFACE_GENERIC,
  INTERFACE_INTRINSIC_OP, INTERFACE_USER_OP, INTERFACE_ABSTRACT,
  INTERFACE_DTIO
};

/* Symbol flavors: these are all mutually exclusive.
   12 elements = 4 bits.  */
enum sym_flavor
{
  FL_UNKNOWN = 0, FL_PROGRAM, FL_BLOCK_DATA, FL_MODULE, FL_VARIABLE,
  FL_PARAMETER, FL_LABEL, FL_PROCEDURE, FL_DERIVED, FL_NAMELIST,
  FL_UNION, FL_STRUCT, FL_VOID
};

/* Procedure types.  7 elements = 3 bits.  */
enum procedure_type
{ PROC_UNKNOWN, PROC_MODULE, PROC_INTERNAL, PROC_DUMMY,
  PROC_INTRINSIC, PROC_ST_FUNCTION, PROC_EXTERNAL
};

/* Intent types.  */
enum sym_intent
{ INTENT_UNKNOWN = 0, INTENT_IN, INTENT_OUT, INTENT_INOUT
};

/* Access types.  */
enum gfc_access
{ ACCESS_UNKNOWN = 0, ACCESS_PUBLIC, ACCESS_PRIVATE
};

/* Flags to keep track of where an interface came from.
   3 elements = 2 bits.  */
enum ifsrc
{ IFSRC_UNKNOWN = 0,	/* Interface unknown, only return type may be known.  */
  IFSRC_DECL,		/* FUNCTION or SUBROUTINE declaration.  */
  IFSRC_IFBODY		/* INTERFACE statement or PROCEDURE statement
			   with explicit interface.  */
};

/* Whether a SAVE attribute was set explicitly or implicitly.  */
enum save_state
{ SAVE_NONE = 0, SAVE_EXPLICIT, SAVE_IMPLICIT
};

/* Strings for all symbol attributes.  We use these for dumping the
   parse tree, in error messages, and also when reading and writing
   modules.  In symbol.c.  */
extern const mstring flavors[];
extern const mstring procedures[];
extern const mstring intents[];
extern const mstring access_types[];
extern const mstring ifsrc_types[];
extern const mstring save_status[];

/* Strings for DTIO procedure names.  In symbol.c.  */
extern const mstring dtio_procs[];

enum dtio_codes
{ DTIO_RF = 0, DTIO_WF, DTIO_RUF, DTIO_WUF };

/* Enumeration of all the generic intrinsic functions.  Used by the
   backend for identification of a function.  */

enum gfc_isym_id
{
  /* GFC_ISYM_NONE is used for intrinsics which will never be seen by
     the backend (e.g. KIND).  */
  GFC_ISYM_NONE = 0,
  GFC_ISYM_ABORT,
  GFC_ISYM_ABS,
  GFC_ISYM_ACCESS,
  GFC_ISYM_ACHAR,
  GFC_ISYM_ACOS,
  GFC_ISYM_ACOSH,
  GFC_ISYM_ADJUSTL,
  GFC_ISYM_ADJUSTR,
  GFC_ISYM_AIMAG,
  GFC_ISYM_AINT,
  GFC_ISYM_ALARM,
  GFC_ISYM_ALL,
  GFC_ISYM_ALLOCATED,
  GFC_ISYM_AND,
  GFC_ISYM_ANINT,
  GFC_ISYM_ANY,
  GFC_ISYM_ASIN,
  GFC_ISYM_ASINH,
  GFC_ISYM_ASSOCIATED,
  GFC_ISYM_ATAN,
  GFC_ISYM_ATAN2,
  GFC_ISYM_ATANH,
  GFC_ISYM_ATOMIC_ADD,
  GFC_ISYM_ATOMIC_AND,
  GFC_ISYM_ATOMIC_CAS,
  GFC_ISYM_ATOMIC_DEF,
  GFC_ISYM_ATOMIC_FETCH_ADD,
  GFC_ISYM_ATOMIC_FETCH_AND,
  GFC_ISYM_ATOMIC_FETCH_OR,
  GFC_ISYM_ATOMIC_FETCH_XOR,
  GFC_ISYM_ATOMIC_OR,
  GFC_ISYM_ATOMIC_REF,
  GFC_ISYM_ATOMIC_XOR,
  GFC_ISYM_BGE,
  GFC_ISYM_BGT,
  GFC_ISYM_BIT_SIZE,
  GFC_ISYM_BLE,
  GFC_ISYM_BLT,
  GFC_ISYM_BTEST,
  GFC_ISYM_CAF_GET,
  GFC_ISYM_CAF_SEND,
  GFC_ISYM_CEILING,
  GFC_ISYM_CHAR,
  GFC_ISYM_CHDIR,
  GFC_ISYM_CHMOD,
  GFC_ISYM_CMPLX,
  GFC_ISYM_CO_BROADCAST,
  GFC_ISYM_CO_MAX,
  GFC_ISYM_CO_MIN,
  GFC_ISYM_CO_REDUCE,
  GFC_ISYM_CO_SUM,
  GFC_ISYM_COMMAND_ARGUMENT_COUNT,
  GFC_ISYM_COMPILER_OPTIONS,
  GFC_ISYM_COMPILER_VERSION,
  GFC_ISYM_COMPLEX,
  GFC_ISYM_CONJG,
  GFC_ISYM_CONVERSION,
  GFC_ISYM_COS,
  GFC_ISYM_COSH,
  GFC_ISYM_COTAN,
  GFC_ISYM_COUNT,
  GFC_ISYM_CPU_TIME,
  GFC_ISYM_CSHIFT,
  GFC_ISYM_CTIME,
  GFC_ISYM_C_ASSOCIATED,
  GFC_ISYM_C_F_POINTER,
  GFC_ISYM_C_F_PROCPOINTER,
  GFC_ISYM_C_FUNLOC,
  GFC_ISYM_C_LOC,
  GFC_ISYM_C_SIZEOF,
  GFC_ISYM_DATE_AND_TIME,
  GFC_ISYM_DBLE,
  GFC_ISYM_DIGITS,
  GFC_ISYM_DIM,
  GFC_ISYM_DOT_PRODUCT,
  GFC_ISYM_DPROD,
  GFC_ISYM_DSHIFTL,
  GFC_ISYM_DSHIFTR,
  GFC_ISYM_DTIME,
  GFC_ISYM_EOSHIFT,
  GFC_ISYM_EPSILON,
  GFC_ISYM_ERF,
  GFC_ISYM_ERFC,
  GFC_ISYM_ERFC_SCALED,
  GFC_ISYM_ETIME,
  GFC_ISYM_EVENT_QUERY,
  GFC_ISYM_EXECUTE_COMMAND_LINE,
  GFC_ISYM_EXIT,
  GFC_ISYM_EXP,
  GFC_ISYM_EXPONENT,
  GFC_ISYM_EXTENDS_TYPE_OF,
  GFC_ISYM_FAILED_IMAGES,
  GFC_ISYM_FDATE,
  GFC_ISYM_FE_RUNTIME_ERROR,
  GFC_ISYM_FGET,
  GFC_ISYM_FGETC,
  GFC_ISYM_FLOOR,
  GFC_ISYM_FLUSH,
  GFC_ISYM_FNUM,
  GFC_ISYM_FPUT,
  GFC_ISYM_FPUTC,
  GFC_ISYM_FRACTION,
  GFC_ISYM_FREE,
  GFC_ISYM_FSEEK,
  GFC_ISYM_FSTAT,
  GFC_ISYM_FTELL,
  GFC_ISYM_TGAMMA,
  GFC_ISYM_GERROR,
  GFC_ISYM_GETARG,
  GFC_ISYM_GET_COMMAND,
  GFC_ISYM_GET_COMMAND_ARGUMENT,
  GFC_ISYM_GETCWD,
  GFC_ISYM_GETENV,
  GFC_ISYM_GET_ENVIRONMENT_VARIABLE,
  GFC_ISYM_GETGID,
  GFC_ISYM_GETLOG,
  GFC_ISYM_GETPID,
  GFC_ISYM_GET_TEAM,
  GFC_ISYM_GETUID,
  GFC_ISYM_GMTIME,
  GFC_ISYM_HOSTNM,
  GFC_ISYM_HUGE,
  GFC_ISYM_HYPOT,
  GFC_ISYM_IACHAR,
  GFC_ISYM_IALL,
  GFC_ISYM_IAND,
  GFC_ISYM_IANY,
  GFC_ISYM_IARGC,
  GFC_ISYM_IBCLR,
  GFC_ISYM_IBITS,
  GFC_ISYM_IBSET,
  GFC_ISYM_ICHAR,
  GFC_ISYM_IDATE,
  GFC_ISYM_IEOR,
  GFC_ISYM_IERRNO,
  GFC_ISYM_IMAGE_INDEX,
  GFC_ISYM_IMAGE_STATUS,
  GFC_ISYM_INDEX,
  GFC_ISYM_INT,
  GFC_ISYM_INT2,
  GFC_ISYM_INT8,
  GFC_ISYM_IOR,
  GFC_ISYM_IPARITY,
  GFC_ISYM_IRAND,
  GFC_ISYM_ISATTY,
  GFC_ISYM_IS_IOSTAT_END,
  GFC_ISYM_IS_IOSTAT_EOR,
  GFC_ISYM_ISNAN,
  GFC_ISYM_ISHFT,
  GFC_ISYM_ISHFTC,
  GFC_ISYM_ITIME,
  GFC_ISYM_J0,
  GFC_ISYM_J1,
  GFC_ISYM_JN,
  GFC_ISYM_JN2,
  GFC_ISYM_KILL,
  GFC_ISYM_KIND,
  GFC_ISYM_LBOUND,
  GFC_ISYM_LCOBOUND,
  GFC_ISYM_LEADZ,
  GFC_ISYM_LEN,
  GFC_ISYM_LEN_TRIM,
  GFC_ISYM_LGAMMA,
  GFC_ISYM_LGE,
  GFC_ISYM_LGT,
  GFC_ISYM_LINK,
  GFC_ISYM_LLE,
  GFC_ISYM_LLT,
  GFC_ISYM_LOC,
  GFC_ISYM_LOG,
  GFC_ISYM_LOG10,
  GFC_ISYM_LOGICAL,
  GFC_ISYM_LONG,
  GFC_ISYM_LSHIFT,
  GFC_ISYM_LSTAT,
  GFC_ISYM_LTIME,
  GFC_ISYM_MALLOC,
  GFC_ISYM_MASKL,
  GFC_ISYM_MASKR,
  GFC_ISYM_MATMUL,
  GFC_ISYM_MAX,
  GFC_ISYM_MAXEXPONENT,
  GFC_ISYM_MAXLOC,
  GFC_ISYM_MAXVAL,
  GFC_ISYM_MCLOCK,
  GFC_ISYM_MCLOCK8,
  GFC_ISYM_MERGE,
  GFC_ISYM_MERGE_BITS,
  GFC_ISYM_MIN,
  GFC_ISYM_MINEXPONENT,
  GFC_ISYM_MINLOC,
  GFC_ISYM_MINVAL,
  GFC_ISYM_MOD,
  GFC_ISYM_MODULO,
  GFC_ISYM_MOVE_ALLOC,
  GFC_ISYM_MVBITS,
  GFC_ISYM_NEAREST,
  GFC_ISYM_NEW_LINE,
  GFC_ISYM_NINT,
  GFC_ISYM_NORM2,
  GFC_ISYM_NOT,
  GFC_ISYM_NULL,
  GFC_ISYM_NUM_IMAGES,
  GFC_ISYM_OR,
  GFC_ISYM_PACK,
  GFC_ISYM_PARITY,
  GFC_ISYM_PERROR,
  GFC_ISYM_POPCNT,
  GFC_ISYM_POPPAR,
  GFC_ISYM_PRECISION,
  GFC_ISYM_PRESENT,
  GFC_ISYM_PRODUCT,
  GFC_ISYM_RADIX,
  GFC_ISYM_RAND,
  GFC_ISYM_RANDOM_NUMBER,
  GFC_ISYM_RANDOM_SEED,
  GFC_ISYM_RANGE,
  GFC_ISYM_RANK,
  GFC_ISYM_REAL,
  GFC_ISYM_RENAME,
  GFC_ISYM_REPEAT,
  GFC_ISYM_RESHAPE,
  GFC_ISYM_RRSPACING,
  GFC_ISYM_RSHIFT,
  GFC_ISYM_SAME_TYPE_AS,
  GFC_ISYM_SC_KIND,
  GFC_ISYM_SCALE,
  GFC_ISYM_SCAN,
  GFC_ISYM_SECNDS,
  GFC_ISYM_SECOND,
  GFC_ISYM_SET_EXPONENT,
  GFC_ISYM_SHAPE,
  GFC_ISYM_SHIFTA,
  GFC_ISYM_SHIFTL,
  GFC_ISYM_SHIFTR,
  GFC_ISYM_BACKTRACE,
  GFC_ISYM_SIGN,
  GFC_ISYM_SIGNAL,
  GFC_ISYM_SI_KIND,
  GFC_ISYM_SIN,
  GFC_ISYM_SINH,
  GFC_ISYM_SIZE,
  GFC_ISYM_SLEEP,
  GFC_ISYM_SIZEOF,
  GFC_ISYM_SPACING,
  GFC_ISYM_SPREAD,
  GFC_ISYM_SQRT,
  GFC_ISYM_SRAND,
  GFC_ISYM_SR_KIND,
  GFC_ISYM_STAT,
  GFC_ISYM_STOPPED_IMAGES,
  GFC_ISYM_STORAGE_SIZE,
  GFC_ISYM_STRIDE,
  GFC_ISYM_SUM,
  GFC_ISYM_SYMLINK,
  GFC_ISYM_SYMLNK,
  GFC_ISYM_SYSTEM,
  GFC_ISYM_SYSTEM_CLOCK,
  GFC_ISYM_TAN,
  GFC_ISYM_TANH,
  GFC_ISYM_TEAM_NUMBER,
  GFC_ISYM_THIS_IMAGE,
  GFC_ISYM_TIME,
  GFC_ISYM_TIME8,
  GFC_ISYM_TINY,
  GFC_ISYM_TRAILZ,
  GFC_ISYM_TRANSFER,
  GFC_ISYM_TRANSPOSE,
  GFC_ISYM_TRIM,
  GFC_ISYM_TTYNAM,
  GFC_ISYM_UBOUND,
  GFC_ISYM_UCOBOUND,
  GFC_ISYM_UMASK,
  GFC_ISYM_UNLINK,
  GFC_ISYM_UNPACK,
  GFC_ISYM_VERIFY,
  GFC_ISYM_XOR,
  GFC_ISYM_Y0,
  GFC_ISYM_Y1,
  GFC_ISYM_YN,
  GFC_ISYM_YN2
};

enum init_local_logical
{
  GFC_INIT_LOGICAL_OFF = 0,
  GFC_INIT_LOGICAL_FALSE,
  GFC_INIT_LOGICAL_TRUE
};

enum init_local_character
{
  GFC_INIT_CHARACTER_OFF = 0,
  GFC_INIT_CHARACTER_ON
};

enum init_local_integer
{
  GFC_INIT_INTEGER_OFF = 0,
  GFC_INIT_INTEGER_ON
};

enum gfc_reverse
{
  GFC_ENABLE_REVERSE,
  GFC_FORWARD_SET,
  GFC_REVERSE_SET,
  GFC_INHIBIT_REVERSE
};

enum gfc_param_spec_type
{
  SPEC_EXPLICIT,
  SPEC_ASSUMED,
  SPEC_DEFERRED
};

/************************* Structures *****************************/

/* Used for keeping things in balanced binary trees.  */
#define BBT_HEADER(self) int priority; struct self *left, *right

#define NAMED_INTCST(a,b,c,d) a,
#define NAMED_KINDARRAY(a,b,c,d) a,
#define NAMED_FUNCTION(a,b,c,d) a,
#define NAMED_SUBROUTINE(a,b,c,d) a,
#define NAMED_DERIVED_TYPE(a,b,c,d) a,
enum iso_fortran_env_symbol
{
  ISOFORTRANENV_INVALID = -1,
#include "iso-fortran-env.def"
  ISOFORTRANENV_LAST, ISOFORTRANENV_NUMBER = ISOFORTRANENV_LAST
};
#undef NAMED_INTCST
#undef NAMED_KINDARRAY
#undef NAMED_FUNCTION
#undef NAMED_SUBROUTINE
#undef NAMED_DERIVED_TYPE

#define NAMED_INTCST(a,b,c,d) a,
#define NAMED_REALCST(a,b,c,d) a,
#define NAMED_CMPXCST(a,b,c,d) a,
#define NAMED_LOGCST(a,b,c) a,
#define NAMED_CHARKNDCST(a,b,c) a,
#define NAMED_CHARCST(a,b,c) a,
#define DERIVED_TYPE(a,b,c) a,
#define NAMED_FUNCTION(a,b,c,d) a,
#define NAMED_SUBROUTINE(a,b,c,d) a,
enum iso_c_binding_symbol
{
  ISOCBINDING_INVALID = -1,
#include "iso-c-binding.def"
  ISOCBINDING_LAST,
  ISOCBINDING_NUMBER = ISOCBINDING_LAST
};
#undef NAMED_INTCST
#undef NAMED_REALCST
#undef NAMED_CMPXCST
#undef NAMED_LOGCST
#undef NAMED_CHARKNDCST
#undef NAMED_CHARCST
#undef DERIVED_TYPE
#undef NAMED_FUNCTION
#undef NAMED_SUBROUTINE

enum intmod_id
{
  INTMOD_NONE = 0, INTMOD_ISO_FORTRAN_ENV, INTMOD_ISO_C_BINDING,
  INTMOD_IEEE_FEATURES, INTMOD_IEEE_EXCEPTIONS, INTMOD_IEEE_ARITHMETIC
};

typedef struct
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  int value;  /* Used for both integer and character values.  */
  bt f90_type;
}
CInteropKind_t;

/* Array of structs, where the structs represent the C interop kinds.
   The list will be implemented based on a hash of the kind name since
   these could be accessed multiple times.
   Declared in trans-types.c as a global, since it's in that file
   that the list is initialized.  */
extern CInteropKind_t c_interop_kinds_table[];


/* Structure and list of supported extension attributes.  */
typedef enum
{
  EXT_ATTR_DLLIMPORT = 0,
  EXT_ATTR_DLLEXPORT,
  EXT_ATTR_STDCALL,
  EXT_ATTR_CDECL,
  EXT_ATTR_FASTCALL,
  EXT_ATTR_NO_ARG_CHECK,
  EXT_ATTR_LAST, EXT_ATTR_NUM = EXT_ATTR_LAST
}
ext_attr_id_t;

typedef struct
{
  const char *name;
  unsigned id;
  const char *middle_end_name;
}
ext_attr_t;

extern const ext_attr_t ext_attr_list[];

/* Symbol attribute structure.  */
typedef struct
{
  /* Variable attributes.  */
  unsigned allocatable:1, dimension:1, codimension:1, external:1, intrinsic:1,
    optional:1, pointer:1, target:1, value:1, volatile_:1, temporary:1,
    dummy:1, result:1, assign:1, threadprivate:1, not_always_present:1,
    implied_index:1, subref_array_pointer:1, proc_pointer:1, asynchronous:1,
    contiguous:1, fe_temp: 1, automatic: 1;

  /* For CLASS containers, the pointer attribute is sometimes set internally
     even though it was not directly specified.  In this case, keep the
     "real" (original) value here.  */
  unsigned class_pointer:1;

  ENUM_BITFIELD (save_state) save:2;

  unsigned data:1,		/* Symbol is named in a DATA statement.  */
    is_protected:1,		/* Symbol has been marked as protected.  */
    use_assoc:1,		/* Symbol has been use-associated.  */
    used_in_submodule:1,	/* Symbol has been use-associated in a
				   submodule. Needed since these entities must
				   be set host associated to be compliant.  */
    use_only:1,			/* Symbol has been use-associated, with ONLY.  */
    use_rename:1,		/* Symbol has been use-associated and renamed.  */
    imported:1,			/* Symbol has been associated by IMPORT.  */
    host_assoc:1;		/* Symbol has been host associated.  */

  unsigned in_namelist:1, in_common:1, in_equivalence:1;
  unsigned function:1, subroutine:1, procedure:1;
  unsigned generic:1, generic_copy:1;
  unsigned implicit_type:1;	/* Type defined via implicit rules.  */
  unsigned untyped:1;		/* No implicit type could be found.  */

  unsigned is_bind_c:1;		/* say if is bound to C.  */
  unsigned extension:8;		/* extension level of a derived type.  */
  unsigned is_class:1;		/* is a CLASS container.  */
  unsigned class_ok:1;		/* is a CLASS object with correct attributes.  */
  unsigned vtab:1;		/* is a derived type vtab, pointed to by CLASS objects.  */
  unsigned vtype:1;		/* is a derived type of a vtab.  */

  /* These flags are both in the typespec and attribute.  The attribute
     list is what gets read from/written to a module file.  The typespec
     is created from a decl being processed.  */
  unsigned is_c_interop:1;	/* It's c interoperable.  */
  unsigned is_iso_c:1;		/* Symbol is from iso_c_binding.  */

  /* Function/subroutine attributes */
  unsigned sequence:1, elemental:1, pure:1, recursive:1;
  unsigned unmaskable:1, masked:1, contained:1, mod_proc:1, abstract:1;

  /* Set if this is a module function or subroutine. Note that it is an
     attribute because it appears as a prefix in the declaration like
     PURE, etc..  */
  unsigned module_procedure:1;

  /* Set if a (public) symbol [e.g. generic name] exposes this symbol,
     which is relevant for private module procedures.  */
  unsigned public_used:1;

  /* This is set if a contained procedure could be declared pure.  This is
     used for certain optimizations that require the result or arguments
     cannot alias.  Note that this is zero for PURE procedures.  */
  unsigned implicit_pure:1;

  /* This is set for a procedure that contains expressions referencing
     arrays coming from outside its namespace.
     This is used to force the creation of a temporary when the LHS of
     an array assignment may be used by an elemental procedure appearing
     on the RHS.  */
  unsigned array_outer_dependency:1;

  /* This is set if the subroutine doesn't return.  Currently, this
     is only possible for intrinsic subroutines.  */
  unsigned noreturn:1;

  /* Set if this procedure is an alternate entry point.  These procedures
     don't have any code associated, and the backend will turn them into
     thunks to the master function.  */
  unsigned entry:1;

  /* Set if this is the master function for a procedure with multiple
     entry points.  */
  unsigned entry_master:1;

  /* Set if this is the master function for a function with multiple
     entry points where characteristics of the entry points differ.  */
  unsigned mixed_entry_master:1;

  /* Set if a function must always be referenced by an explicit interface.  */
  unsigned always_explicit:1;

  /* Set if the symbol is generated and, hence, standard violations
     shouldn't be flaged.  */
  unsigned artificial:1;

  /* Set if the symbol has been referenced in an expression.  No further
     modification of type or type parameters is permitted.  */
  unsigned referenced:1;

  /* Set if this is the symbol for the main program.  */
  unsigned is_main_program:1;

  /* Mutually exclusive multibit attributes.  */
  ENUM_BITFIELD (gfc_access) access:2;
  ENUM_BITFIELD (sym_intent) intent:2;
  ENUM_BITFIELD (sym_flavor) flavor:4;
  ENUM_BITFIELD (ifsrc) if_source:2;

  ENUM_BITFIELD (procedure_type) proc:3;

  /* Special attributes for Cray pointers, pointees.  */
  unsigned cray_pointer:1, cray_pointee:1;

  /* The symbol is a derived type with allocatable components, pointer
     components or private components, procedure pointer components,
     possibly nested.  zero_comp is true if the derived type has no
     component at all.  defined_assign_comp is true if the derived
     type or a (sub-)component has a typebound defined assignment.
     unlimited_polymorphic flags the type of the container for these
     entities.  */
  unsigned alloc_comp:1, pointer_comp:1, proc_pointer_comp:1,
	   private_comp:1, zero_comp:1, coarray_comp:1, lock_comp:1,
	   event_comp:1, defined_assign_comp:1, unlimited_polymorphic:1,
	   has_dtio_procs:1, caf_token:1;

  /* This is a temporary selector for SELECT TYPE or an associate
     variable for SELECT_TYPE or ASSOCIATE.  */
  unsigned select_type_temporary:1, associate_var:1;

  /* These are the attributes required for parameterized derived
     types.  */
  unsigned pdt_kind:1, pdt_len:1, pdt_type:1, pdt_template:1,
	   pdt_array:1, pdt_string:1;

  /* This is omp_{out,in,priv,orig} artificial variable in
     !$OMP DECLARE REDUCTION.  */
  unsigned omp_udr_artificial_var:1;

  /* Mentioned in OMP DECLARE TARGET.  */
  unsigned omp_declare_target:1;
  unsigned omp_declare_target_link:1;

  /* Mentioned in OACC DECLARE.  */
  unsigned oacc_declare_create:1;
  unsigned oacc_declare_copyin:1;
  unsigned oacc_declare_deviceptr:1;
  unsigned oacc_declare_device_resident:1;
  unsigned oacc_declare_link:1;

  /* This is an OpenACC acclerator function at level N - 1  */
  unsigned oacc_function:3;

  /* Attributes set by compiler extensions (!GCC$ ATTRIBUTES).  */
  unsigned ext_attr:EXT_ATTR_NUM;

  /* The namespace where the attribute has been set.  */
  struct gfc_namespace *volatile_ns, *asynchronous_ns;
}
symbol_attribute;


/* We need to store source lines as sequences of multibyte source
   characters. We define here a type wide enough to hold any multibyte
   source character, just like libcpp does.  A 32-bit type is enough.  */

#if HOST_BITS_PER_INT >= 32
typedef unsigned int gfc_char_t;
#elif HOST_BITS_PER_LONG >= 32
typedef unsigned long gfc_char_t;
#elif defined(HAVE_LONG_LONG) && (HOST_BITS_PER_LONGLONG >= 32)
typedef unsigned long long gfc_char_t;
#else
# error "Cannot find an integer type with at least 32 bits"
#endif


/* The following three structures are used to identify a location in
   the sources.

   gfc_file is used to maintain a tree of the source files and how
   they include each other

   gfc_linebuf holds a single line of source code and information
   which file it resides in

   locus point to the sourceline and the character in the source
   line.
*/

typedef struct gfc_file
{
  struct gfc_file *next, *up;
  int inclusion_line, line;
  char *filename;
} gfc_file;

typedef struct gfc_linebuf
{
  source_location location;
  struct gfc_file *file;
  struct gfc_linebuf *next;

  int truncated;
  bool dbg_emitted;

  gfc_char_t line[1];
} gfc_linebuf;

#define gfc_linebuf_header_size (offsetof (gfc_linebuf, line))

#define gfc_linebuf_linenum(LBUF) (LOCATION_LINE ((LBUF)->location))

typedef struct
{
  gfc_char_t *nextc;
  gfc_linebuf *lb;
} locus;

/* In order for the "gfc" format checking to work correctly, you must
   have declared a typedef locus first.  */
#if GCC_VERSION >= 4001
#define ATTRIBUTE_GCC_GFC(m, n) __attribute__ ((__format__ (__gcc_gfc__, m, n))) ATTRIBUTE_NONNULL(m)
#else
#define ATTRIBUTE_GCC_GFC(m, n) ATTRIBUTE_NONNULL(m)
#endif


/* Suppress error messages or re-enable them.  */

void gfc_push_suppress_errors (void);
void gfc_pop_suppress_errors (void);


/* Character length structures hold the expression that gives the
   length of a character variable.  We avoid putting these into
   gfc_typespec because doing so prevents us from doing structure
   copies and forces us to deallocate any typespecs we create, as well
   as structures that contain typespecs.  They also can have multiple
   character typespecs pointing to them.

   These structures form a singly linked list within the current
   namespace and are deallocated with the namespace.  It is possible to
   end up with gfc_charlen structures that have nothing pointing to them.  */

typedef struct gfc_charlen
{
  struct gfc_expr *length;
  struct gfc_charlen *next;
  bool length_from_typespec; /* Length from explicit array ctor typespec?  */
  tree backend_decl;
  tree passed_length; /* Length argument explicitly passed.  */

  int resolved;
}
gfc_charlen;

#define gfc_get_charlen() XCNEW (gfc_charlen)

/* Type specification structure.  */
typedef struct
{
  bt type;
  int kind;

  union
  {
    struct gfc_symbol *derived;	/* For derived types only.  */
    gfc_charlen *cl;		/* For character types only.  */
    int pad;			/* For hollerith types only.  */
  }
  u;

  struct gfc_symbol *interface;	/* For PROCEDURE declarations.  */
  int is_c_interop;
  int is_iso_c;
  bt f90_type;
  bool deferred;
  gfc_symbol *interop_kind;
}
gfc_typespec;

/* Array specification.  */
typedef struct
{
  int rank;	/* A scalar has a rank of 0, an assumed-rank array has -1.  */
  int corank;
  array_type type, cotype;
  struct gfc_expr *lower[GFC_MAX_DIMENSIONS], *upper[GFC_MAX_DIMENSIONS];

  /* These two fields are used with the Cray Pointer extension.  */
  bool cray_pointee; /* True iff this spec belongs to a cray pointee.  */
  bool cp_was_assumed; /* AS_ASSUMED_SIZE cp arrays are converted to
			AS_EXPLICIT, but we want to remember that we
			did this.  */

  bool resolved;
}
gfc_array_spec;

#define gfc_get_array_spec() XCNEW (gfc_array_spec)


/* Components of derived types.  */
typedef struct gfc_component
{
  const char *name;
  gfc_typespec ts;

  symbol_attribute attr;
  gfc_array_spec *as;

  tree backend_decl;
  /* Used to cache a FIELD_DECL matching this same component
     but applied to a different backend containing type that was
     generated by gfc_nonrestricted_type.  */
  tree norestrict_decl;
  locus loc;
  struct gfc_expr *initializer;
  /* Used in parameterized derived type declarations to store parameterized
     kind expressions.  */
  struct gfc_expr *kind_expr;
  struct gfc_actual_arglist *param_list;

  struct gfc_component *next;

  /* Needed for procedure pointer components.  */
  struct gfc_typebound_proc *tb;
  /* When allocatable/pointer and in a coarray the associated token.  */
  tree caf_token;
}
gfc_component;

#define gfc_get_component() XCNEW (gfc_component)

/* Formal argument lists are lists of symbols.  */
typedef struct gfc_formal_arglist
{
  /* Symbol representing the argument at this position in the arglist.  */
  struct gfc_symbol *sym;
  /* Points to the next formal argument.  */
  struct gfc_formal_arglist *next;
}
gfc_formal_arglist;

#define gfc_get_formal_arglist() XCNEW (gfc_formal_arglist)


/* The gfc_actual_arglist structure is for actual arguments and
   for type parameter specification lists.  */
typedef struct gfc_actual_arglist
{
  const char *name;
  /* Alternate return label when the expr member is null.  */
  struct gfc_st_label *label;

  /* This is set to the type of an eventual omitted optional
     argument. This is used to determine if a hidden string length
     argument has to be added to a function call.  */
  bt missing_arg_type;

  gfc_param_spec_type spec_type;

  struct gfc_expr *expr;
  struct gfc_actual_arglist *next;
}
gfc_actual_arglist;

#define gfc_get_actual_arglist() XCNEW (gfc_actual_arglist)


/* Because a symbol can belong to multiple namelists, they must be
   linked externally to the symbol itself.  */
typedef struct gfc_namelist
{
  struct gfc_symbol *sym;
  struct gfc_namelist *next;
}
gfc_namelist;

#define gfc_get_namelist() XCNEW (gfc_namelist)

/* Likewise to gfc_namelist, but contains expressions.  */
typedef struct gfc_expr_list
{
  struct gfc_expr *expr;
  struct gfc_expr_list *next;
}
gfc_expr_list;

#define gfc_get_expr_list() XCNEW (gfc_expr_list)

enum gfc_omp_reduction_op
{
  OMP_REDUCTION_NONE = -1,
  OMP_REDUCTION_PLUS = INTRINSIC_PLUS,
  OMP_REDUCTION_MINUS = INTRINSIC_MINUS,
  OMP_REDUCTION_TIMES = INTRINSIC_TIMES,
  OMP_REDUCTION_AND = INTRINSIC_AND,
  OMP_REDUCTION_OR = INTRINSIC_OR,
  OMP_REDUCTION_EQV = INTRINSIC_EQV,
  OMP_REDUCTION_NEQV = INTRINSIC_NEQV,
  OMP_REDUCTION_MAX = GFC_INTRINSIC_END,
  OMP_REDUCTION_MIN,
  OMP_REDUCTION_IAND,
  OMP_REDUCTION_IOR,
  OMP_REDUCTION_IEOR,
  OMP_REDUCTION_USER
};

enum gfc_omp_depend_op
{
  OMP_DEPEND_IN,
  OMP_DEPEND_OUT,
  OMP_DEPEND_INOUT,
  OMP_DEPEND_SINK_FIRST,
  OMP_DEPEND_SINK
};

enum gfc_omp_map_op
{
  OMP_MAP_ALLOC,
  OMP_MAP_TO,
  OMP_MAP_FROM,
  OMP_MAP_TOFROM,
  OMP_MAP_DELETE,
  OMP_MAP_FORCE_ALLOC,
  OMP_MAP_FORCE_TO,
  OMP_MAP_FORCE_FROM,
  OMP_MAP_FORCE_TOFROM,
  OMP_MAP_FORCE_PRESENT,
  OMP_MAP_FORCE_DEVICEPTR,
  OMP_MAP_DEVICE_RESIDENT,
  OMP_MAP_LINK,
  OMP_MAP_RELEASE,
  OMP_MAP_ALWAYS_TO,
  OMP_MAP_ALWAYS_FROM,
  OMP_MAP_ALWAYS_TOFROM
};

enum gfc_omp_linear_op
{
  OMP_LINEAR_DEFAULT,
  OMP_LINEAR_REF,
  OMP_LINEAR_VAL,
  OMP_LINEAR_UVAL
};

/* For use in OpenMP clauses in case we need extra information
   (aligned clause alignment, linear clause step, etc.).  */

typedef struct gfc_omp_namelist
{
  struct gfc_symbol *sym;
  struct gfc_expr *expr;
  union
    {
      gfc_omp_reduction_op reduction_op;
      gfc_omp_depend_op depend_op;
      gfc_omp_map_op map_op;
      gfc_omp_linear_op linear_op;
      struct gfc_common_head *common;
    } u;
  struct gfc_omp_namelist_udr *udr;
  struct gfc_omp_namelist *next;
  locus where;
}
gfc_omp_namelist;

#define gfc_get_omp_namelist() XCNEW (gfc_omp_namelist)

enum
{
  OMP_LIST_FIRST,
  OMP_LIST_PRIVATE = OMP_LIST_FIRST,
  OMP_LIST_FIRSTPRIVATE,
  OMP_LIST_LASTPRIVATE,
  OMP_LIST_COPYPRIVATE,
  OMP_LIST_SHARED,
  OMP_LIST_COPYIN,
  OMP_LIST_UNIFORM,
  OMP_LIST_ALIGNED,
  OMP_LIST_LINEAR,
  OMP_LIST_DEPEND,
  OMP_LIST_MAP,
  OMP_LIST_TO,
  OMP_LIST_FROM,
  OMP_LIST_REDUCTION,
  OMP_LIST_DEVICE_RESIDENT,
  OMP_LIST_LINK,
  OMP_LIST_USE_DEVICE,
  OMP_LIST_CACHE,
  OMP_LIST_IS_DEVICE_PTR,
  OMP_LIST_USE_DEVICE_PTR,
  OMP_LIST_NUM
};

/* Because a symbol can belong to multiple namelists, they must be
   linked externally to the symbol itself.  */

enum gfc_omp_sched_kind
{
  OMP_SCHED_NONE,
  OMP_SCHED_STATIC,
  OMP_SCHED_DYNAMIC,
  OMP_SCHED_GUIDED,
  OMP_SCHED_RUNTIME,
  OMP_SCHED_AUTO
};

enum gfc_omp_default_sharing
{
  OMP_DEFAULT_UNKNOWN,
  OMP_DEFAULT_NONE,
  OMP_DEFAULT_PRIVATE,
  OMP_DEFAULT_SHARED,
  OMP_DEFAULT_FIRSTPRIVATE,
  OMP_DEFAULT_PRESENT
};

enum gfc_omp_proc_bind_kind
{
  OMP_PROC_BIND_UNKNOWN,
  OMP_PROC_BIND_MASTER,
  OMP_PROC_BIND_SPREAD,
  OMP_PROC_BIND_CLOSE
};

enum gfc_omp_cancel_kind
{
  OMP_CANCEL_UNKNOWN,
  OMP_CANCEL_PARALLEL,
  OMP_CANCEL_SECTIONS,
  OMP_CANCEL_DO,
  OMP_CANCEL_TASKGROUP
};

enum gfc_omp_if_kind
{
  OMP_IF_PARALLEL,
  OMP_IF_TASK,
  OMP_IF_TASKLOOP,
  OMP_IF_TARGET,
  OMP_IF_TARGET_DATA,
  OMP_IF_TARGET_UPDATE,
  OMP_IF_TARGET_ENTER_DATA,
  OMP_IF_TARGET_EXIT_DATA,
  OMP_IF_LAST
};

typedef struct gfc_omp_clauses
{
  struct gfc_expr *if_expr;
  struct gfc_expr *final_expr;
  struct gfc_expr *num_threads;
  gfc_omp_namelist *lists[OMP_LIST_NUM];
  enum gfc_omp_sched_kind sched_kind;
  struct gfc_expr *chunk_size;
  enum gfc_omp_default_sharing default_sharing;
  int collapse, orderedc;
  bool nowait, ordered, untied, mergeable;
  bool inbranch, notinbranch, defaultmap, nogroup;
  bool sched_simd, sched_monotonic, sched_nonmonotonic;
  bool simd, threads, depend_source;
  enum gfc_omp_cancel_kind cancel;
  enum gfc_omp_proc_bind_kind proc_bind;
  struct gfc_expr *safelen_expr;
  struct gfc_expr *simdlen_expr;
  struct gfc_expr *num_teams;
  struct gfc_expr *device;
  struct gfc_expr *thread_limit;
  struct gfc_expr *grainsize;
  struct gfc_expr *hint;
  struct gfc_expr *num_tasks;
  struct gfc_expr *priority;
  struct gfc_expr *if_exprs[OMP_IF_LAST];
  enum gfc_omp_sched_kind dist_sched_kind;
  struct gfc_expr *dist_chunk_size;
  const char *critical_name;

  /* OpenACC. */
  struct gfc_expr *async_expr;
  struct gfc_expr *gang_static_expr;
  struct gfc_expr *gang_num_expr;
  struct gfc_expr *worker_expr;
  struct gfc_expr *vector_expr;
  struct gfc_expr *num_gangs_expr;
  struct gfc_expr *num_workers_expr;
  struct gfc_expr *vector_length_expr;
  gfc_expr_list *wait_list;
  gfc_expr_list *tile_list;
  unsigned async:1, gang:1, worker:1, vector:1, seq:1, independent:1;
  unsigned wait:1, par_auto:1, gang_static:1;
  locus loc;

}
gfc_omp_clauses;

#define gfc_get_omp_clauses() XCNEW (gfc_omp_clauses)


/* Node in the linked list used for storing !$oacc declare constructs.  */

typedef struct gfc_oacc_declare
{
  struct gfc_oacc_declare *next;
  bool module_var;
  gfc_omp_clauses *clauses;
  locus loc;
}
gfc_oacc_declare;

#define gfc_get_oacc_declare() XCNEW (gfc_oacc_declare)


/* Node in the linked list used for storing !$omp declare simd constructs.  */

typedef struct gfc_omp_declare_simd
{
  struct gfc_omp_declare_simd *next;
  locus where; /* Where the !$omp declare simd construct occurred.  */

  gfc_symbol *proc_name;

  gfc_omp_clauses *clauses;
}
gfc_omp_declare_simd;
#define gfc_get_omp_declare_simd() XCNEW (gfc_omp_declare_simd)

typedef struct gfc_omp_udr
{
  struct gfc_omp_udr *next;
  locus where; /* Where the !$omp declare reduction construct occurred.  */

  const char *name;
  gfc_typespec ts;
  gfc_omp_reduction_op rop;

  struct gfc_symbol *omp_out;
  struct gfc_symbol *omp_in;
  struct gfc_namespace *combiner_ns;

  struct gfc_symbol *omp_priv;
  struct gfc_symbol *omp_orig;
  struct gfc_namespace *initializer_ns;
}
gfc_omp_udr;
#define gfc_get_omp_udr() XCNEW (gfc_omp_udr)

typedef struct gfc_omp_namelist_udr
{
  struct gfc_omp_udr *udr;
  struct gfc_code *combiner;
  struct gfc_code *initializer;
}
gfc_omp_namelist_udr;
#define gfc_get_omp_namelist_udr() XCNEW (gfc_omp_namelist_udr)

/* The gfc_st_label structure is a BBT attached to a namespace that
   records the usage of statement labels within that space.  */

typedef struct gfc_st_label
{
  BBT_HEADER(gfc_st_label);

  int value;

  gfc_sl_type defined, referenced;

  struct gfc_expr *format;

  tree backend_decl;

  locus where;

  gfc_namespace *ns;
}
gfc_st_label;


/* gfc_interface()-- Interfaces are lists of symbols strung together.  */
typedef struct gfc_interface
{
  struct gfc_symbol *sym;
  locus where;
  struct gfc_interface *next;
}
gfc_interface;

#define gfc_get_interface() XCNEW (gfc_interface)

/* User operator nodes.  These are like stripped down symbols.  */
typedef struct
{
  const char *name;

  gfc_interface *op;
  struct gfc_namespace *ns;
  gfc_access access;
}
gfc_user_op;


/* A list of specific bindings that are associated with a generic spec.  */
typedef struct gfc_tbp_generic
{
  /* The parser sets specific_st, upon resolution we look for the corresponding
     gfc_typebound_proc and set specific for further use.  */
  struct gfc_symtree* specific_st;
  struct gfc_typebound_proc* specific;

  struct gfc_tbp_generic* next;
  bool is_operator;
}
gfc_tbp_generic;

#define gfc_get_tbp_generic() XCNEW (gfc_tbp_generic)


/* Data needed for type-bound procedures.  */
typedef struct gfc_typebound_proc
{
  locus where; /* Where the PROCEDURE/GENERIC definition was.  */

  union
  {
    struct gfc_symtree* specific; /* The interface if DEFERRED.  */
    gfc_tbp_generic* generic;
  }
  u;

  gfc_access access;
  const char* pass_arg; /* Argument-name for PASS.  NULL if not specified.  */

  /* The overridden type-bound proc (or GENERIC with this name in the
     parent-type) or NULL if non.  */
  struct gfc_typebound_proc* overridden;

  /* Once resolved, we use the position of pass_arg in the formal arglist of
     the binding-target procedure to identify it.  The first argument has
     number 1 here, the second 2, and so on.  */
  unsigned pass_arg_num;

  unsigned nopass:1; /* Whether we have NOPASS (PASS otherwise).  */
  unsigned non_overridable:1;
  unsigned deferred:1;
  unsigned is_generic:1;
  unsigned function:1, subroutine:1;
  unsigned error:1; /* Ignore it, when an error occurred during resolution.  */
  unsigned ppc:1;
}
gfc_typebound_proc;


/* Symbol nodes.  These are important things.  They are what the
   standard refers to as "entities".  The possibly multiple names that
   refer to the same entity are accomplished by a binary tree of
   symtree structures that is balanced by the red-black method-- more
   than one symtree node can point to any given symbol.  */

typedef struct gfc_symbol
{
  const char *name;	/* Primary name, before renaming */
  const char *module;	/* Module this symbol came from */
  locus declared_at;

  gfc_typespec ts;
  symbol_attribute attr;

  /* The formal member points to the formal argument list if the
     symbol is a function or subroutine name.  If the symbol is a
     generic name, the generic member points to the list of
     interfaces.  */

  gfc_interface *generic;
  gfc_access component_access;

  gfc_formal_arglist *formal;
  struct gfc_namespace *formal_ns;
  struct gfc_namespace *f2k_derived;

  /* List of PDT parameter expressions  */
  struct gfc_actual_arglist *param_list;

  struct gfc_expr *value;	/* Parameter/Initializer value */
  gfc_array_spec *as;
  struct gfc_symbol *result;	/* function result symbol */
  gfc_component *components;	/* Derived type components */

  /* Defined only for Cray pointees; points to their pointer.  */
  struct gfc_symbol *cp_pointer;

  int entry_id;			/* Used in resolve.c for entries.  */

  /* CLASS hashed name for declared and dynamic types in the class.  */
  int hash_value;

  struct gfc_symbol *common_next;	/* Links for COMMON syms */

  /* This is only used for pointer comparisons to check if symbols
     are in the same common block.
     In opposition to common_block, the common_head pointer takes into account
     equivalences: if A is in a common block C and A and B are in equivalence,
     then both A and B have common_head pointing to C, while A's common_block
     points to C and B's is NULL.  */
  struct gfc_common_head* common_head;

  /* Make sure setup code for dummy arguments is generated in the correct
     order.  */
  int dummy_order;

  gfc_namelist *namelist, *namelist_tail;

  /* Change management fields.  Symbols that might be modified by the
     current statement have the mark member nonzero.  Of these symbols,
     symbols with old_symbol equal to NULL are symbols created within
     the current statement.  Otherwise, old_symbol points to a copy of
     the old symbol. gfc_new is used in symbol.c to flag new symbols.  */
  struct gfc_symbol *old_symbol;
  unsigned mark:1, gfc_new:1;

  /* The tlink field is used in the front end to carry the module
     declaration of separate module procedures so that the characteristics
     can be compared with the corresponding declaration in a submodule. In
     translation this field carries a linked list of symbols that require
     deferred initialization.  */
  struct gfc_symbol *tlink;

  /* Nonzero if all equivalences associated with this symbol have been
     processed.  */
  unsigned equiv_built:1;
  /* Set if this variable is used as an index name in a FORALL.  */
  unsigned forall_index:1;
  /* Set if the symbol is used in a function result specification .  */
  unsigned fn_result_spec:1;
  /* Used to avoid multiple resolutions of a single symbol.  */
  unsigned resolved:1;
  /* Set if this is a module function or subroutine with the
     abreviated declaration in a submodule.  */
  unsigned abr_modproc_decl:1;

  int refs;
  struct gfc_namespace *ns;	/* namespace containing this symbol */

  tree backend_decl;

  /* Identity of the intrinsic module the symbol comes from, or
     INTMOD_NONE if it's not imported from a intrinsic module.  */
  intmod_id from_intmod;
  /* Identity of the symbol from intrinsic modules, from enums maintained
     separately by each intrinsic module.  Used together with from_intmod,
     it uniquely identifies a symbol from an intrinsic module.  */
  int intmod_sym_id;

  /* This may be repetitive, since the typespec now has a binding
     label field.  */
  const char* binding_label;
  /* Store a reference to the common_block, if this symbol is in one.  */
  struct gfc_common_head *common_block;

  /* Link to corresponding association-list if this is an associate name.  */
  struct gfc_association_list *assoc;
}
gfc_symbol;


struct gfc_undo_change_set
{
  vec<gfc_symbol *> syms;
  vec<gfc_typebound_proc *> tbps;
  gfc_undo_change_set *previous;
};


/* This structure is used to keep track of symbols in common blocks.  */
typedef struct gfc_common_head
{
  locus where;
  char use_assoc, saved, threadprivate;
  unsigned char omp_declare_target : 1;
  unsigned char omp_declare_target_link : 1;
  char name[GFC_MAX_SYMBOL_LEN + 1];
  struct gfc_symbol *head;
  const char* binding_label;
  int is_bind_c;
  int refs;
}
gfc_common_head;

#define gfc_get_common_head() XCNEW (gfc_common_head)


/* A list of all the alternate entry points for a procedure.  */

typedef struct gfc_entry_list
{
  /* The symbol for this entry point.  */
  gfc_symbol *sym;
  /* The zero-based id of this entry point.  */
  int id;
  /* The LABEL_EXPR marking this entry point.  */
  tree label;
  /* The next item in the list.  */
  struct gfc_entry_list *next;
}
gfc_entry_list;

#define gfc_get_entry_list() XCNEW (gfc_entry_list)

/* Lists of rename info for the USE statement.  */

typedef struct gfc_use_rename
{
  char local_name[GFC_MAX_SYMBOL_LEN + 1], use_name[GFC_MAX_SYMBOL_LEN + 1];
  struct gfc_use_rename *next;
  int found;
  gfc_intrinsic_op op;
  locus where;
}
gfc_use_rename;

#define gfc_get_use_rename() XCNEW (gfc_use_rename);

/* A list of all USE statements in a namespace.  */

typedef struct gfc_use_list
{
  const char *module_name;
  const char *submodule_name;
  bool intrinsic;
  bool non_intrinsic;
  bool only_flag;
  struct gfc_use_rename *rename;
  locus where;
  /* Next USE statement.  */
  struct gfc_use_list *next;
}
gfc_use_list;

#define gfc_get_use_list() XCNEW (gfc_use_list)

/* Within a namespace, symbols are pointed to by symtree nodes that
   are linked together in a balanced binary tree.  There can be
   several symtrees pointing to the same symbol node via USE
   statements.  */

typedef struct gfc_symtree
{
  BBT_HEADER (gfc_symtree);
  const char *name;
  int ambiguous;
  union
  {
    gfc_symbol *sym;		/* Symbol associated with this node */
    gfc_user_op *uop;
    gfc_common_head *common;
    gfc_typebound_proc *tb;
    gfc_omp_udr *omp_udr;
  }
  n;
}
gfc_symtree;

/* A linked list of derived types in the namespace.  */
typedef struct gfc_dt_list
{
  struct gfc_symbol *derived;
  struct gfc_dt_list *next;
}
gfc_dt_list;

#define gfc_get_dt_list() XCNEW (gfc_dt_list)

  /* A list of all derived types.  */
  extern gfc_dt_list *gfc_derived_types;

typedef struct gfc_oacc_routine_name
{
  struct gfc_symbol *sym;
  struct gfc_omp_clauses *clauses;
  struct gfc_oacc_routine_name *next;
}
gfc_oacc_routine_name;

#define gfc_get_oacc_routine_name() XCNEW (gfc_oacc_routine_name)

/* A namespace describes the contents of procedure, module, interface block
   or BLOCK construct.  */
/* ??? Anything else use these?  */

typedef struct gfc_namespace
{
  /* Tree containing all the symbols in this namespace.  */
  gfc_symtree *sym_root;
  /* Tree containing all the user-defined operators in the namespace.  */
  gfc_symtree *uop_root;
  /* Tree containing all the common blocks.  */
  gfc_symtree *common_root;
  /* Tree containing all the OpenMP user defined reductions.  */
  gfc_symtree *omp_udr_root;

  /* Tree containing type-bound procedures.  */
  gfc_symtree *tb_sym_root;
  /* Type-bound user operators.  */
  gfc_symtree *tb_uop_root;
  /* For derived-types, store type-bound intrinsic operators here.  */
  gfc_typebound_proc *tb_op[GFC_INTRINSIC_OPS];
  /* Linked list of finalizer procedures.  */
  struct gfc_finalizer *finalizers;

  /* If set_flag[letter] is set, an implicit type has been set for letter.  */
  int set_flag[GFC_LETTERS];
  /* Keeps track of the implicit types associated with the letters.  */
  gfc_typespec default_type[GFC_LETTERS];
  /* Store the positions of IMPLICIT statements.  */
  locus implicit_loc[GFC_LETTERS];

  /* If this is a namespace of a procedure, this points to the procedure.  */
  struct gfc_symbol *proc_name;
  /* If this is the namespace of a unit which contains executable
     code, this points to it.  */
  struct gfc_code *code;

  /* Points to the equivalences set up in this namespace.  */
  struct gfc_equiv *equiv, *old_equiv;

  /* Points to the equivalence groups produced by trans_common.  */
  struct gfc_equiv_list *equiv_lists;

  gfc_interface *op[GFC_INTRINSIC_OPS];

  /* Points to the parent namespace, i.e. the namespace of a module or
     procedure in which the procedure belonging to this namespace is
     contained. The parent namespace points to this namespace either
     directly via CONTAINED, or indirectly via the chain built by
     SIBLING.  */
  struct gfc_namespace *parent;
  /* CONTAINED points to the first contained namespace. Sibling
     namespaces are chained via SIBLING.  */
  struct gfc_namespace  *contained, *sibling;

  gfc_common_head blank_common;
  gfc_access default_access, operator_access[GFC_INTRINSIC_OPS];

  gfc_st_label *st_labels;
  /* This list holds information about all the data initializers in
     this namespace.  */
  struct gfc_data *data, *old_data;

  /* !$ACC DECLARE.  */
  gfc_oacc_declare *oacc_declare;

  /* !$ACC ROUTINE clauses.  */
  gfc_omp_clauses *oacc_routine_clauses;

  /* !$ACC ROUTINE names.  */
  gfc_oacc_routine_name *oacc_routine_names;

  gfc_charlen *cl_list;

  gfc_dt_list *derived_types;

  int save_all, seen_save, seen_implicit_none;

  /* Normally we don't need to refcount namespaces.  However when we read
     a module containing a function with multiple entry points, this
     will appear as several functions with the same formal namespace.  */
  int refs;

  /* A list of all alternate entry points to this procedure (or NULL).  */
  gfc_entry_list *entries;

  /* A list of USE statements in this namespace.  */
  gfc_use_list *use_stmts;

  /* Linked list of !$omp declare simd constructs.  */
  struct gfc_omp_declare_simd *omp_declare_simd;

  /* Set to 1 if namespace is a BLOCK DATA program unit.  */
  unsigned is_block_data:1;

  /* Set to 1 if namespace is an interface body with "IMPORT" used.  */
  unsigned has_import_set:1;

  /* Set to 1 if the namespace uses "IMPLICT NONE (export)".  */
  unsigned has_implicit_none_export:1;

  /* Set to 1 if resolved has been called for this namespace.
     Holds -1 during resolution.  */
  signed resolved:2;

  /* Set when resolve_types has been called for this namespace.  */
  unsigned types_resolved:1;

  /* Set to 1 if code has been generated for this namespace.  */
  unsigned translated:1;

  /* Set to 1 if symbols in this namespace should be 'construct entities',
     i.e. for BLOCK local variables.  */
  unsigned construct_entities:1;

  /* Set to 1 for !$OMP DECLARE REDUCTION namespaces.  */
  unsigned omp_udr_ns:1;

  /* Set to 1 for !$ACC ROUTINE namespaces.  */
  unsigned oacc_routine:1;
}
gfc_namespace;

extern gfc_namespace *gfc_current_ns;
extern gfc_namespace *gfc_global_ns_list;

/* Global symbols are symbols of global scope. Currently we only use
   this to detect collisions already when parsing.
   TODO: Extend to verify procedure calls.  */

enum gfc_symbol_type
{
  GSYM_UNKNOWN=1, GSYM_PROGRAM, GSYM_FUNCTION, GSYM_SUBROUTINE,
  GSYM_MODULE, GSYM_COMMON, GSYM_BLOCK_DATA
};

typedef struct gfc_gsymbol
{
  BBT_HEADER(gfc_gsymbol);

  const char *name;
  const char *sym_name;
  const char *mod_name;
  const char *binding_label;
  enum gfc_symbol_type type;

  int defined, used;
  locus where;
  gfc_namespace *ns;
}
gfc_gsymbol;

extern gfc_gsymbol *gfc_gsym_root;

/* Information on interfaces being built.  */
typedef struct
{
  interface_type type;
  gfc_symbol *sym;
  gfc_namespace *ns;
  gfc_user_op *uop;
  gfc_intrinsic_op op;
}
gfc_interface_info;

extern gfc_interface_info current_interface;


/* Array reference.  */

enum gfc_array_ref_dimen_type
{
  DIMEN_ELEMENT = 1, DIMEN_RANGE, DIMEN_VECTOR, DIMEN_STAR, DIMEN_THIS_IMAGE, DIMEN_UNKNOWN
};

typedef struct gfc_array_ref
{
  ar_type type;
  int dimen;			/* # of components in the reference */
  int codimen;
  bool in_allocate;		/* For coarray checks. */
  gfc_expr *team;
  gfc_expr *stat;
  locus where;
  gfc_array_spec *as;

  locus c_where[GFC_MAX_DIMENSIONS];	/* All expressions can be NULL */
  struct gfc_expr *start[GFC_MAX_DIMENSIONS], *end[GFC_MAX_DIMENSIONS],
    *stride[GFC_MAX_DIMENSIONS];

  enum gfc_array_ref_dimen_type dimen_type[GFC_MAX_DIMENSIONS];
}
gfc_array_ref;

#define gfc_get_array_ref() XCNEW (gfc_array_ref)


/* Component reference nodes.  A variable is stored as an expression
   node that points to the base symbol.  After that, a singly linked
   list of component reference nodes gives the variable's complete
   resolution.  The array_ref component may be present and comes
   before the component component.  */

enum ref_type
  { REF_ARRAY, REF_COMPONENT, REF_SUBSTRING };

typedef struct gfc_ref
{
  ref_type type;

  union
  {
    struct gfc_array_ref ar;

    struct
    {
      gfc_component *component;
      gfc_symbol *sym;
    }
    c;

    struct
    {
      struct gfc_expr *start, *end;	/* Substring */
      gfc_charlen *length;
    }
    ss;

  }
  u;

  struct gfc_ref *next;
}
gfc_ref;

#define gfc_get_ref() XCNEW (gfc_ref)


/* Structures representing intrinsic symbols and their arguments lists.  */
typedef struct gfc_intrinsic_arg
{
  char name[GFC_MAX_SYMBOL_LEN + 1];

  gfc_typespec ts;
  unsigned optional:1, value:1;
  ENUM_BITFIELD (sym_intent) intent:2;
  gfc_actual_arglist *actual;

  struct gfc_intrinsic_arg *next;

}
gfc_intrinsic_arg;


/* Specifies the various kinds of check functions used to verify the
   argument lists of intrinsic functions. fX with X an integer refer
   to check functions of intrinsics with X arguments. f1m is used for
   the MAX and MIN intrinsics which can have an arbitrary number of
   arguments, f4ml is used for the MINLOC and MAXLOC intrinsics as
   these have special semantics.  */

typedef union
{
  bool (*f0)(void);
  bool (*f1)(struct gfc_expr *);
  bool (*f1m)(gfc_actual_arglist *);
  bool (*f2)(struct gfc_expr *, struct gfc_expr *);
  bool (*f3)(struct gfc_expr *, struct gfc_expr *, struct gfc_expr *);
  bool (*f5ml)(gfc_actual_arglist *);
  bool (*f3red)(gfc_actual_arglist *);
  bool (*f4)(struct gfc_expr *, struct gfc_expr *, struct gfc_expr *,
	    struct gfc_expr *);
  bool (*f5)(struct gfc_expr *, struct gfc_expr *, struct gfc_expr *,
	    struct gfc_expr *, struct gfc_expr *);
}
gfc_check_f;

/* Like gfc_check_f, these specify the type of the simplification
   function associated with an intrinsic. The fX are just like in
   gfc_check_f. cc is used for type conversion functions.  */

typedef union
{
  struct gfc_expr *(*f0)(void);
  struct gfc_expr *(*f1)(struct gfc_expr *);
  struct gfc_expr *(*f2)(struct gfc_expr *, struct gfc_expr *);
  struct gfc_expr *(*f3)(struct gfc_expr *, struct gfc_expr *,
			 struct gfc_expr *);
  struct gfc_expr *(*f4)(struct gfc_expr *, struct gfc_expr *,
			 struct gfc_expr *, struct gfc_expr *);
  struct gfc_expr *(*f5)(struct gfc_expr *, struct gfc_expr *,
			 struct gfc_expr *, struct gfc_expr *,
			 struct gfc_expr *);
  struct gfc_expr *(*cc)(struct gfc_expr *, bt, int);
}
gfc_simplify_f;

/* Again like gfc_check_f, these specify the type of the resolution
   function associated with an intrinsic. The fX are just like in
   gfc_check_f. f1m is used for MIN and MAX, s1 is used for abort().  */

typedef union
{
  void (*f0)(struct gfc_expr *);
  void (*f1)(struct gfc_expr *, struct gfc_expr *);
  void (*f1m)(struct gfc_expr *, struct gfc_actual_arglist *);
  void (*f2)(struct gfc_expr *, struct gfc_expr *, struct gfc_expr *);
  void (*f3)(struct gfc_expr *, struct gfc_expr *, struct gfc_expr *,
	     struct gfc_expr *);
  void (*f4)(struct gfc_expr *, struct gfc_expr *, struct gfc_expr *,
	     struct gfc_expr *, struct gfc_expr *);
  void (*f5)(struct gfc_expr *, struct gfc_expr *, struct gfc_expr *,
	     struct gfc_expr *, struct gfc_expr *, struct gfc_expr *);
  void (*s1)(struct gfc_code *);
}
gfc_resolve_f;


typedef struct gfc_intrinsic_sym
{
  const char *name, *lib_name;
  gfc_intrinsic_arg *formal;
  gfc_typespec ts;
  unsigned elemental:1, inquiry:1, transformational:1, pure:1,
    generic:1, specific:1, actual_ok:1, noreturn:1, conversion:1,
    from_module:1, vararg:1;

  int standard;

  gfc_simplify_f simplify;
  gfc_check_f check;
  gfc_resolve_f resolve;
  struct gfc_intrinsic_sym *specific_head, *next;
  gfc_isym_id id;

}
gfc_intrinsic_sym;


/* Expression nodes.  The expression node types deserve explanations,
   since the last couple can be easily misconstrued:

   EXPR_OP         Operator node pointing to one or two other nodes
   EXPR_FUNCTION   Function call, symbol points to function's name
   EXPR_CONSTANT   A scalar constant: Logical, String, Real, Int or Complex
   EXPR_VARIABLE   An Lvalue with a root symbol and possible reference list
		   which expresses structure, array and substring refs.
   EXPR_NULL       The NULL pointer value (which also has a basic type).
   EXPR_SUBSTRING  A substring of a constant string
   EXPR_STRUCTURE  A structure constructor
   EXPR_ARRAY      An array constructor.
   EXPR_COMPCALL   Function (or subroutine) call of a procedure pointer
		   component or type-bound procedure.  */

#include <mpfr.h>
#include <mpc.h>
#define GFC_RND_MODE GMP_RNDN
#define GFC_MPC_RND_MODE MPC_RNDNN

typedef splay_tree gfc_constructor_base;


/* This should be an unsigned variable of type size_t.  But to handle
   compiling to a 64-bit target from a 32-bit host, we need to use a
   HOST_WIDE_INT.  Also, occasionally the string length field is used
   as a flag with values -1 and -2, see e.g. gfc_add_assign_aux_vars.
   So it needs to be signed.  */
typedef HOST_WIDE_INT gfc_charlen_t;

typedef struct gfc_expr
{
  expr_t expr_type;

  gfc_typespec ts;	/* These two refer to the overall expression */

  int rank;		/* 0 indicates a scalar, -1 an assumed-rank array.  */
  mpz_t *shape;		/* Can be NULL if shape is unknown at compile time */

  /* Nonnull for functions and structure constructors, may also used to hold the
     base-object for component calls.  */
  gfc_symtree *symtree;

  gfc_ref *ref;

  locus where;

  /* Used to store the base expression in component calls, when the expression
     is not a variable.  */
  struct gfc_expr *base_expr;

  /* is_boz is true if the integer is regarded as BOZ bit pattern and is_snan
     denotes a signalling not-a-number.  */
  unsigned int is_boz : 1, is_snan : 1;

  /* Sometimes, when an error has been emitted, it is necessary to prevent
      it from recurring.  */
  unsigned int error : 1;

  /* Mark an expression where a user operator has been substituted by
     a function call in interface.c(gfc_extend_expr).  */
  unsigned int user_operator : 1;

  /* Mark an expression as being a MOLD argument of ALLOCATE.  */
  unsigned int mold : 1;

  /* Will require finalization after use.  */
  unsigned int must_finalize : 1;

  /* If an expression comes from a Hollerith constant or compile-time
     evaluation of a transfer statement, it may have a prescribed target-
     memory representation, and these cannot always be backformed from
     the value.  */
  struct
  {
    gfc_charlen_t length;
    char *string;
  }
  representation;

  union
  {
    int logical;

    io_kind iokind;

    mpz_t integer;

    mpfr_t real;

    mpc_t complex;

    struct
    {
      gfc_intrinsic_op op;
      gfc_user_op *uop;
      struct gfc_expr *op1, *op2;
    }
    op;

    struct
    {
      gfc_actual_arglist *actual;
      const char *name;	/* Points to the ultimate name of the function */
      gfc_intrinsic_sym *isym;
      gfc_symbol *esym;
    }
    function;

    struct
    {
      gfc_actual_arglist* actual;
      const char* name;
      /* Base-object, whose component was called.  NULL means that it should
	 be taken from symtree/ref.  */
      struct gfc_expr* base_object;
      gfc_typebound_proc* tbp; /* Should overlap with esym.  */

      /* For type-bound operators, we want to call PASS procedures but already
	 have the full arglist; mark this, so that it is not extended by the
	 PASS argument.  */
      unsigned ignore_pass:1;

      /* Do assign-calls rather than calls, that is appropriate dependency
	 checking.  */
      unsigned assign:1;
    }
    compcall;

    struct
    {
      gfc_charlen_t length;
      gfc_char_t *string;
    }
    character;

    gfc_constructor_base constructor;
  }
  value;

  /* Used to store PDT expression lists associated with expressions.  */
  gfc_actual_arglist *param_list;

}
gfc_expr;


#define gfc_get_shape(rank) (XCNEWVEC (mpz_t, (rank)))

/* Structures for information associated with different kinds of
   numbers.  The first set of integer parameters define all there is
   to know about a particular kind.  The rest of the elements are
   computed from the first elements.  */

typedef struct
{
  /* Values really representable by the target.  */
  mpz_t huge, pedantic_min_int, min_int;

  int kind, radix, digits, bit_size, range;

  /* True if the C type of the given name maps to this precision.
     Note that more than one bit can be set.  */
  unsigned int c_char : 1;
  unsigned int c_short : 1;
  unsigned int c_int : 1;
  unsigned int c_long : 1;
  unsigned int c_long_long : 1;
}
gfc_integer_info;

extern gfc_integer_info gfc_integer_kinds[];


typedef struct
{
  int kind, bit_size;

  /* True if the C++ type bool, C99 type _Bool, maps to this precision.  */
  unsigned int c_bool : 1;
}
gfc_logical_info;

extern gfc_logical_info gfc_logical_kinds[];


typedef struct
{
  mpfr_t epsilon, huge, tiny, subnormal;
  int kind, radix, digits, min_exponent, max_exponent;
  int range, precision;

  /* The precision of the type as reported by GET_MODE_PRECISION.  */
  int mode_precision;

  /* True if the C type of the given name maps to this precision.
     Note that more than one bit can be set.  */
  unsigned int c_float : 1;
  unsigned int c_double : 1;
  unsigned int c_long_double : 1;
  unsigned int c_float128 : 1;
}
gfc_real_info;

extern gfc_real_info gfc_real_kinds[];

typedef struct
{
  int kind, bit_size;
  const char *name;
}
gfc_character_info;

extern gfc_character_info gfc_character_kinds[];


/* Equivalence structures.  Equivalent lvalues are linked along the
   *eq pointer, equivalence sets are strung along the *next node.  */
typedef struct gfc_equiv
{
  struct gfc_equiv *next, *eq;
  gfc_expr *expr;
  const char *module;
  int used;
}
gfc_equiv;

#define gfc_get_equiv() XCNEW (gfc_equiv)

/* Holds a single equivalence member after processing.  */
typedef struct gfc_equiv_info
{
  gfc_symbol *sym;
  HOST_WIDE_INT offset;
  HOST_WIDE_INT length;
  struct gfc_equiv_info *next;
} gfc_equiv_info;

/* Holds equivalence groups, after they have been processed.  */
typedef struct gfc_equiv_list
{
  gfc_equiv_info *equiv;
  struct gfc_equiv_list *next;
} gfc_equiv_list;

/* gfc_case stores the selector list of a case statement.  The *low
   and *high pointers can point to the same expression in the case of
   a single value.  If *high is NULL, the selection is from *low
   upwards, if *low is NULL the selection is *high downwards.

   This structure has separate fields to allow single and double linked
   lists of CASEs at the same time.  The singe linked list along the NEXT
   field is a list of cases for a single CASE label.  The double linked
   list along the LEFT/RIGHT fields is used to detect overlap and to
   build a table of the cases for SELECT constructs with a CHARACTER
   case expression.  */

typedef struct gfc_case
{
  /* Where we saw this case.  */
  locus where;
  int n;

  /* Case range values.  If (low == high), it's a single value.  If one of
     the labels is NULL, it's an unbounded case.  If both are NULL, this
     represents the default case.  */
  gfc_expr *low, *high;

  /* Only used for SELECT TYPE.  */
  gfc_typespec ts;

  /* Next case label in the list of cases for a single CASE label.  */
  struct gfc_case *next;

  /* Used for detecting overlap, and for code generation.  */
  struct gfc_case *left, *right;

  /* True if this case label can never be matched.  */
  int unreachable;
}
gfc_case;

#define gfc_get_case() XCNEW (gfc_case)


typedef struct
{
  gfc_expr *var, *start, *end, *step;
  unsigned short unroll;
}
gfc_iterator;

#define gfc_get_iterator() XCNEW (gfc_iterator)


/* Allocation structure for ALLOCATE, DEALLOCATE and NULLIFY statements.  */

typedef struct gfc_alloc
{
  gfc_expr *expr;
  struct gfc_alloc *next;
}
gfc_alloc;

#define gfc_get_alloc() XCNEW (gfc_alloc)


typedef struct
{
  gfc_expr *unit, *file, *status, *access, *form, *recl,
    *blank, *position, *action, *delim, *pad, *iostat, *iomsg, *convert,
    *decimal, *encoding, *round, *sign, *asynchronous, *id, *newunit,
    *share, *cc;
  char readonly;
  gfc_st_label *err;
}
gfc_open;


typedef struct
{
  gfc_expr *unit, *status, *iostat, *iomsg;
  gfc_st_label *err;
}
gfc_close;


typedef struct
{
  gfc_expr *unit, *iostat, *iomsg;
  gfc_st_label *err;
}
gfc_filepos;


typedef struct
{
  gfc_expr *unit, *file, *iostat, *exist, *opened, *number, *named,
    *name, *access, *sequential, *direct, *form, *formatted,
    *unformatted, *recl, *nextrec, *blank, *position, *action, *read,
    *write, *readwrite, *delim, *pad, *iolength, *iomsg, *convert, *strm_pos,
    *asynchronous, *decimal, *encoding, *pending, *round, *sign, *size, *id,
    *iqstream, *share, *cc;

  gfc_st_label *err;

}
gfc_inquire;


typedef struct
{
  gfc_expr *unit, *iostat, *iomsg, *id;
  gfc_st_label *err, *end, *eor;
}
gfc_wait;


typedef struct
{
  gfc_expr *io_unit, *format_expr, *rec, *advance, *iostat, *size, *iomsg,
	   *id, *pos, *asynchronous, *blank, *decimal, *delim, *pad, *round,
	   *sign, *extra_comma, *dt_io_kind, *udtio;
  char dec_ext;

  gfc_symbol *namelist;
  /* A format_label of `format_asterisk' indicates the "*" format */
  gfc_st_label *format_label;
  gfc_st_label *err, *end, *eor;

  locus eor_where, end_where, err_where;
}
gfc_dt;


typedef struct gfc_forall_iterator
{
  gfc_expr *var, *start, *end, *stride;
  struct gfc_forall_iterator *next;
}
gfc_forall_iterator;


/* Linked list to store associations in an ASSOCIATE statement.  */

typedef struct gfc_association_list
{
  struct gfc_association_list *next;

  /* Whether this is association to a variable that can be changed; otherwise,
     it's association to an expression and the name may not be used as
     lvalue.  */
  unsigned variable:1;

  /* True if this struct is currently only linked to from a gfc_symbol rather
     than as part of a real list in gfc_code->ext.block.assoc.  This may
     happen for SELECT TYPE temporaries and must be considered
     for memory handling.  */
  unsigned dangling:1;

  /* True when the rank of the target expression is guessed during parsing.  */
  unsigned rankguessed:1;

  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_symtree *st; /* Symtree corresponding to name.  */
  locus where;

  gfc_expr *target;
}
gfc_association_list;
#define gfc_get_association_list() XCNEW (gfc_association_list)


/* Executable statements that fill gfc_code structures.  */
enum gfc_exec_op
{
  EXEC_NOP = 1, EXEC_END_NESTED_BLOCK, EXEC_END_BLOCK, EXEC_ASSIGN,
  EXEC_LABEL_ASSIGN, EXEC_POINTER_ASSIGN, EXEC_CRITICAL, EXEC_ERROR_STOP,
  EXEC_GOTO, EXEC_CALL, EXEC_COMPCALL, EXEC_ASSIGN_CALL, EXEC_RETURN,
  EXEC_ENTRY, EXEC_PAUSE, EXEC_STOP, EXEC_CONTINUE, EXEC_INIT_ASSIGN,
  EXEC_IF, EXEC_ARITHMETIC_IF, EXEC_DO, EXEC_DO_CONCURRENT, EXEC_DO_WHILE,
  EXEC_SELECT, EXEC_BLOCK, EXEC_FORALL, EXEC_WHERE, EXEC_CYCLE, EXEC_EXIT,
  EXEC_CALL_PPC, EXEC_ALLOCATE, EXEC_DEALLOCATE, EXEC_END_PROCEDURE,
  EXEC_SELECT_TYPE, EXEC_SYNC_ALL, EXEC_SYNC_MEMORY, EXEC_SYNC_IMAGES,
  EXEC_OPEN, EXEC_CLOSE, EXEC_WAIT,
  EXEC_READ, EXEC_WRITE, EXEC_IOLENGTH, EXEC_TRANSFER, EXEC_DT_END,
  EXEC_BACKSPACE, EXEC_ENDFILE, EXEC_INQUIRE, EXEC_REWIND, EXEC_FLUSH,
  EXEC_FORM_TEAM, EXEC_CHANGE_TEAM, EXEC_END_TEAM, EXEC_SYNC_TEAM,
  EXEC_LOCK, EXEC_UNLOCK, EXEC_EVENT_POST, EXEC_EVENT_WAIT, EXEC_FAIL_IMAGE,
  EXEC_OACC_KERNELS_LOOP, EXEC_OACC_PARALLEL_LOOP, EXEC_OACC_ROUTINE,
  EXEC_OACC_PARALLEL, EXEC_OACC_KERNELS, EXEC_OACC_DATA, EXEC_OACC_HOST_DATA,
  EXEC_OACC_LOOP, EXEC_OACC_UPDATE, EXEC_OACC_WAIT, EXEC_OACC_CACHE,
  EXEC_OACC_ENTER_DATA, EXEC_OACC_EXIT_DATA, EXEC_OACC_ATOMIC,
  EXEC_OACC_DECLARE,
  EXEC_OMP_CRITICAL, EXEC_OMP_DO, EXEC_OMP_FLUSH, EXEC_OMP_MASTER,
  EXEC_OMP_ORDERED, EXEC_OMP_PARALLEL, EXEC_OMP_PARALLEL_DO,
  EXEC_OMP_PARALLEL_SECTIONS, EXEC_OMP_PARALLEL_WORKSHARE,
  EXEC_OMP_SECTIONS, EXEC_OMP_SINGLE, EXEC_OMP_WORKSHARE,
  EXEC_OMP_ATOMIC, EXEC_OMP_BARRIER, EXEC_OMP_END_NOWAIT,
  EXEC_OMP_END_SINGLE, EXEC_OMP_TASK, EXEC_OMP_TASKWAIT,
  EXEC_OMP_TASKYIELD, EXEC_OMP_CANCEL, EXEC_OMP_CANCELLATION_POINT,
  EXEC_OMP_TASKGROUP, EXEC_OMP_SIMD, EXEC_OMP_DO_SIMD,
  EXEC_OMP_PARALLEL_DO_SIMD, EXEC_OMP_TARGET, EXEC_OMP_TARGET_DATA,
  EXEC_OMP_TEAMS, EXEC_OMP_DISTRIBUTE, EXEC_OMP_DISTRIBUTE_SIMD,
  EXEC_OMP_DISTRIBUTE_PARALLEL_DO, EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD,
  EXEC_OMP_TARGET_TEAMS, EXEC_OMP_TEAMS_DISTRIBUTE,
  EXEC_OMP_TEAMS_DISTRIBUTE_SIMD, EXEC_OMP_TARGET_TEAMS_DISTRIBUTE,
  EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD,
  EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO,
  EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO,
  EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD,
  EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD,
  EXEC_OMP_TARGET_UPDATE, EXEC_OMP_END_CRITICAL,
  EXEC_OMP_TARGET_ENTER_DATA, EXEC_OMP_TARGET_EXIT_DATA,
  EXEC_OMP_TARGET_PARALLEL, EXEC_OMP_TARGET_PARALLEL_DO,
  EXEC_OMP_TARGET_PARALLEL_DO_SIMD, EXEC_OMP_TARGET_SIMD,
  EXEC_OMP_TASKLOOP, EXEC_OMP_TASKLOOP_SIMD
};

enum gfc_omp_atomic_op
{
  GFC_OMP_ATOMIC_UPDATE = 0,
  GFC_OMP_ATOMIC_READ = 1,
  GFC_OMP_ATOMIC_WRITE = 2,
  GFC_OMP_ATOMIC_CAPTURE = 3,
  GFC_OMP_ATOMIC_MASK = 3,
  GFC_OMP_ATOMIC_SEQ_CST = 4,
  GFC_OMP_ATOMIC_SWAP = 8
};

typedef struct gfc_code
{
  gfc_exec_op op;

  struct gfc_code *block, *next;
  locus loc;

  gfc_st_label *here, *label1, *label2, *label3;
  gfc_symtree *symtree;
  gfc_expr *expr1, *expr2, *expr3, *expr4;
  /* A name isn't sufficient to identify a subroutine, we need the actual
     symbol for the interface definition.
  const char *sub_name;  */
  gfc_symbol *resolved_sym;
  gfc_intrinsic_sym *resolved_isym;

  union
  {
    gfc_actual_arglist *actual;
    gfc_iterator *iterator;

    struct
    {
      gfc_typespec ts;
      gfc_alloc *list;
      /* Take the array specification from expr3 to allocate arrays
	 without an explicit array specification.  */
      unsigned arr_spec_from_expr3:1;
    }
    alloc;

    struct
    {
      gfc_namespace *ns;
      gfc_association_list *assoc;
      gfc_case *case_list;
    }
    block;

    gfc_open *open;
    gfc_close *close;
    gfc_filepos *filepos;
    gfc_inquire *inquire;
    gfc_wait *wait;
    gfc_dt *dt;
    gfc_forall_iterator *forall_iterator;
    struct gfc_code *which_construct;
    int stop_code;
    gfc_entry_list *entry;
    gfc_oacc_declare *oacc_declare;
    gfc_omp_clauses *omp_clauses;
    const char *omp_name;
    gfc_omp_namelist *omp_namelist;
    bool omp_bool;
    gfc_omp_atomic_op omp_atomic;
  }
  ext;		/* Points to additional structures required by statement */

  /* Cycle and break labels in constructs.  */
  tree cycle_label;
  tree exit_label;
}
gfc_code;


/* Storage for DATA statements.  */
typedef struct gfc_data_variable
{
  gfc_expr *expr;
  gfc_iterator iter;
  struct gfc_data_variable *list, *next;
}
gfc_data_variable;


typedef struct gfc_data_value
{
  mpz_t repeat;
  gfc_expr *expr;
  struct gfc_data_value *next;
}
gfc_data_value;


typedef struct gfc_data
{
  gfc_data_variable *var;
  gfc_data_value *value;
  locus where;

  struct gfc_data *next;
}
gfc_data;


/* Structure for holding compile options */
typedef struct
{
  char *module_dir;
  gfc_source_form source_form;
  int max_continue_fixed;
  int max_continue_free;
  int max_identifier_length;

  int max_errors;

  int flag_preprocessed;
  int flag_d_lines;
  int flag_init_integer;
  int flag_init_integer_value;
  int flag_init_logical;
  int flag_init_character;
  char flag_init_character_value;

  int fpe;
  int fpe_summary;
  int rtcheck;

  int warn_std;
  int allow_std;
}
gfc_option_t;

extern gfc_option_t gfc_option;

/* Constructor nodes for array and structure constructors.  */
typedef struct gfc_constructor
{
  gfc_constructor_base base;
  mpz_t offset;               /* Offset within a constructor, used as
				 key within base. */

  gfc_expr *expr;
  gfc_iterator *iterator;
  locus where;

  union
  {
     gfc_component *component; /* Record the component being initialized.  */
  }
  n;
  mpz_t repeat; /* Record the repeat number of initial values in data
		  statement like "data a/5*10/".  */
}
gfc_constructor;


typedef struct iterator_stack
{
  gfc_symtree *variable;
  mpz_t value;
  struct iterator_stack *prev;
}
iterator_stack;
extern iterator_stack *iter_stack;


/* Used for (possibly nested) SELECT TYPE statements.  */
typedef struct gfc_select_type_stack
{
  gfc_symbol *selector;			/* Current selector variable.  */
  gfc_symtree *tmp;			/* Current temporary variable.  */
  struct gfc_select_type_stack *prev;	/* Previous element on stack.  */
}
gfc_select_type_stack;
extern gfc_select_type_stack *select_type_stack;
#define gfc_get_select_type_stack() XCNEW (gfc_select_type_stack)


/* Node in the linked list used for storing finalizer procedures.  */

typedef struct gfc_finalizer
{
  struct gfc_finalizer* next;
  locus where; /* Where the FINAL declaration occurred.  */

  /* Up to resolution, we want the gfc_symbol, there we lookup the corresponding
     symtree and later need only that.  This way, we can access and call the
     finalizers from every context as they should be "always accessible".  I
     don't make this a union because we need the information whether proc_sym is
     still referenced or not for dereferencing it on deleting a gfc_finalizer
     structure.  */
  gfc_symbol*  proc_sym;
  gfc_symtree* proc_tree;
}
gfc_finalizer;
#define gfc_get_finalizer() XCNEW (gfc_finalizer)


/************************ Function prototypes *************************/

/* decl.c */
bool gfc_in_match_data (void);
match gfc_match_char_spec (gfc_typespec *);
extern int directive_unroll;

/* Handling Parameterized Derived Types  */
bool gfc_insert_kind_parameter_exprs (gfc_expr *);
bool gfc_insert_parameter_exprs (gfc_expr *, gfc_actual_arglist *);
match gfc_get_pdt_instance (gfc_actual_arglist *, gfc_symbol **,
			    gfc_actual_arglist **);

/* scanner.c */
void gfc_scanner_done_1 (void);
void gfc_scanner_init_1 (void);

void gfc_add_include_path (const char *, bool, bool, bool);
void gfc_add_intrinsic_modules_path (const char *);
void gfc_release_include_path (void);
FILE *gfc_open_included_file (const char *, bool, bool);

int gfc_at_end (void);
int gfc_at_eof (void);
int gfc_at_bol (void);
int gfc_at_eol (void);
void gfc_advance_line (void);
int gfc_check_include (void);
int gfc_define_undef_line (void);

int gfc_wide_is_printable (gfc_char_t);
int gfc_wide_is_digit (gfc_char_t);
int gfc_wide_fits_in_byte (gfc_char_t);
gfc_char_t gfc_wide_tolower (gfc_char_t);
gfc_char_t gfc_wide_toupper (gfc_char_t);
size_t gfc_wide_strlen (const gfc_char_t *);
int gfc_wide_strncasecmp (const gfc_char_t *, const char *, size_t);
gfc_char_t *gfc_wide_memset (gfc_char_t *, gfc_char_t, size_t);
char *gfc_widechar_to_char (const gfc_char_t *, int);
gfc_char_t *gfc_char_to_widechar (const char *);

#define gfc_get_wide_string(n) XCNEWVEC (gfc_char_t, n)

void gfc_skip_comments (void);
gfc_char_t gfc_next_char_literal (gfc_instring);
gfc_char_t gfc_next_char (void);
char gfc_next_ascii_char (void);
gfc_char_t gfc_peek_char (void);
char gfc_peek_ascii_char (void);
void gfc_error_recovery (void);
void gfc_gobble_whitespace (void);
bool gfc_new_file (void);
const char * gfc_read_orig_filename (const char *, const char **);

extern gfc_source_form gfc_current_form;
extern const char *gfc_source_file;
extern locus gfc_current_locus;

void gfc_start_source_files (void);
void gfc_end_source_files (void);

/* misc.c */
void gfc_clear_ts (gfc_typespec *);
FILE *gfc_open_file (const char *);
const char *gfc_basic_typename (bt);
const char *gfc_typename (gfc_typespec *);
const char *gfc_op2string (gfc_intrinsic_op);
const char *gfc_code2string (const mstring *, int);
int gfc_string2code (const mstring *, const char *);
const char *gfc_intent_string (sym_intent);

void gfc_init_1 (void);
void gfc_init_2 (void);
void gfc_done_1 (void);
void gfc_done_2 (void);

int get_c_kind (const char *, CInteropKind_t *);

const char *gfc_closest_fuzzy_match (const char *, char **);
static inline void
vec_push (char **&optr, size_t &osz, const char *elt)
{
  /* {auto,}vec.safe_push () replacement.  Don't ask..  */
  // if (strlen (elt) < 4) return; premature optimization: eliminated by cutoff
  optr = XRESIZEVEC (char *, optr, osz + 2);
  optr[osz] = CONST_CAST (char *, elt);
  optr[++osz] = NULL;
}

HOST_WIDE_INT gfc_mpz_get_hwi (mpz_t);
void gfc_mpz_set_hwi (mpz_t, const HOST_WIDE_INT);

/* options.c */
unsigned int gfc_option_lang_mask (void);
void gfc_init_options_struct (struct gcc_options *);
void gfc_init_options (unsigned int,
		       struct cl_decoded_option *);
bool gfc_handle_option (size_t, const char *, int, int, location_t,
			const struct cl_option_handlers *);
bool gfc_post_options (const char **);
char *gfc_get_option_string (void);

/* f95-lang.c */
void gfc_maybe_initialize_eh (void);

/* iresolve.c */
const char * gfc_get_string (const char *, ...) ATTRIBUTE_PRINTF_1;
bool gfc_find_sym_in_expr (gfc_symbol *, gfc_expr *);

/* error.c */
void gfc_error_init_1 (void);
void gfc_diagnostics_init (void);
void gfc_diagnostics_finish (void);
void gfc_buffer_error (bool);

const char *gfc_print_wide_char (gfc_char_t);

bool gfc_warning (int opt, const char *, ...) ATTRIBUTE_GCC_GFC(2,3);
bool gfc_warning_now (int opt, const char *, ...) ATTRIBUTE_GCC_GFC(2,3);
bool gfc_warning_internal (int opt, const char *, ...) ATTRIBUTE_GCC_GFC(2,3);
bool gfc_warning_now_at (location_t loc, int opt, const char *gmsgid, ...)
  ATTRIBUTE_GCC_GFC(3,4);

void gfc_clear_warning (void);
void gfc_warning_check (void);

void gfc_error_opt (int opt, const char *, ...) ATTRIBUTE_GCC_GFC(2,3);
void gfc_error (const char *, ...) ATTRIBUTE_GCC_GFC(1,2);
void gfc_error_now (const char *, ...) ATTRIBUTE_GCC_GFC(1,2);
void gfc_fatal_error (const char *, ...) ATTRIBUTE_NORETURN ATTRIBUTE_GCC_GFC(1,2);
void gfc_internal_error (const char *, ...) ATTRIBUTE_NORETURN ATTRIBUTE_GCC_GFC(1,2);
void gfc_clear_error (void);
bool gfc_error_check (void);
bool gfc_error_flag_test (void);

notification gfc_notification_std (int);
bool gfc_notify_std (int, const char *, ...) ATTRIBUTE_GCC_GFC(2,3);

/* A general purpose syntax error.  */
#define gfc_syntax_error(ST)	\
  gfc_error ("Syntax error in %s statement at %C", gfc_ascii_statement (ST));

#include "pretty-print.h"  /* For output_buffer.  */
struct gfc_error_buffer
{
  bool flag;
  output_buffer buffer;
  gfc_error_buffer(void) : flag(false), buffer() {}
};

void gfc_push_error (gfc_error_buffer *);
void gfc_pop_error (gfc_error_buffer *);
void gfc_free_error (gfc_error_buffer *);

void gfc_get_errors (int *, int *);
void gfc_errors_to_warnings (bool);

/* arith.c */
void gfc_arith_init_1 (void);
void gfc_arith_done_1 (void);
arith gfc_check_integer_range (mpz_t p, int kind);
bool gfc_check_character_range (gfc_char_t, int);

/* trans-types.c */
bool gfc_check_any_c_kind (gfc_typespec *);
int gfc_validate_kind (bt, int, bool);
int gfc_get_int_kind_from_width_isofortranenv (int size);
int gfc_get_real_kind_from_width_isofortranenv (int size);
tree gfc_get_union_type (gfc_symbol *);
tree gfc_get_derived_type (gfc_symbol * derived, int codimen = 0);
extern int gfc_index_integer_kind;
extern int gfc_default_integer_kind;
extern int gfc_max_integer_kind;
extern int gfc_default_real_kind;
extern int gfc_default_double_kind;
extern int gfc_default_character_kind;
extern int gfc_default_logical_kind;
extern int gfc_default_complex_kind;
extern int gfc_c_int_kind;
extern int gfc_atomic_int_kind;
extern int gfc_atomic_logical_kind;
extern int gfc_intio_kind;
extern int gfc_charlen_int_kind;
extern int gfc_size_kind;
extern int gfc_numeric_storage_size;
extern int gfc_character_storage_size;

#define gfc_logical_4_kind 4
#define gfc_integer_4_kind 4

/* symbol.c */
void gfc_clear_new_implicit (void);
bool gfc_add_new_implicit_range (int, int);
bool gfc_merge_new_implicit (gfc_typespec *);
void gfc_set_implicit_none (bool, bool, locus *);
void gfc_check_function_type (gfc_namespace *);
bool gfc_is_intrinsic_typename (const char *);

gfc_typespec *gfc_get_default_type (const char *, gfc_namespace *);
bool gfc_set_default_type (gfc_symbol *, int, gfc_namespace *);

void gfc_set_sym_referenced (gfc_symbol *);

bool gfc_add_attribute (symbol_attribute *, locus *);
bool gfc_add_ext_attribute (symbol_attribute *, ext_attr_id_t, locus *);
bool gfc_add_allocatable (symbol_attribute *, locus *);
bool gfc_add_codimension (symbol_attribute *, const char *, locus *);
bool gfc_add_contiguous (symbol_attribute *, const char *, locus *);
bool gfc_add_dimension (symbol_attribute *, const char *, locus *);
bool gfc_add_external (symbol_attribute *, locus *);
bool gfc_add_intrinsic (symbol_attribute *, locus *);
bool gfc_add_optional (symbol_attribute *, locus *);
bool gfc_add_kind (symbol_attribute *, locus *);
bool gfc_add_len (symbol_attribute *, locus *);
bool gfc_add_pointer (symbol_attribute *, locus *);
bool gfc_add_cray_pointer (symbol_attribute *, locus *);
bool gfc_add_cray_pointee (symbol_attribute *, locus *);
match gfc_mod_pointee_as (gfc_array_spec *);
bool gfc_add_protected (symbol_attribute *, const char *, locus *);
bool gfc_add_result (symbol_attribute *, const char *, locus *);
bool gfc_add_automatic (symbol_attribute *, const char *, locus *);
bool gfc_add_save (symbol_attribute *, save_state, const char *, locus *);
bool gfc_add_threadprivate (symbol_attribute *, const char *, locus *);
bool gfc_add_omp_declare_target (symbol_attribute *, const char *, locus *);
bool gfc_add_omp_declare_target_link (symbol_attribute *, const char *,
				      locus *);
bool gfc_add_saved_common (symbol_attribute *, locus *);
bool gfc_add_target (symbol_attribute *, locus *);
bool gfc_add_dummy (symbol_attribute *, const char *, locus *);
bool gfc_add_generic (symbol_attribute *, const char *, locus *);
bool gfc_add_common (symbol_attribute *, locus *);
bool gfc_add_in_common (symbol_attribute *, const char *, locus *);
bool gfc_add_in_equivalence (symbol_attribute *, const char *, locus *);
bool gfc_add_data (symbol_attribute *, const char *, locus *);
bool gfc_add_in_namelist (symbol_attribute *, const char *, locus *);
bool gfc_add_sequence (symbol_attribute *, const char *, locus *);
bool gfc_add_elemental (symbol_attribute *, locus *);
bool gfc_add_pure (symbol_attribute *, locus *);
bool gfc_add_recursive (symbol_attribute *, locus *);
bool gfc_add_function (symbol_attribute *, const char *, locus *);
bool gfc_add_subroutine (symbol_attribute *, const char *, locus *);
bool gfc_add_volatile (symbol_attribute *, const char *, locus *);
bool gfc_add_asynchronous (symbol_attribute *, const char *, locus *);
bool gfc_add_proc (symbol_attribute *attr, const char *name, locus *where);
bool gfc_add_abstract (symbol_attribute* attr, locus* where);

bool gfc_add_access (symbol_attribute *, gfc_access, const char *, locus *);
bool gfc_add_is_bind_c (symbol_attribute *, const char *, locus *, int);
bool gfc_add_extension (symbol_attribute *, locus *);
bool gfc_add_value (symbol_attribute *, const char *, locus *);
bool gfc_add_flavor (symbol_attribute *, sym_flavor, const char *, locus *);
bool gfc_add_entry (symbol_attribute *, const char *, locus *);
bool gfc_add_procedure (symbol_attribute *, procedure_type,
		       const char *, locus *);
bool gfc_add_intent (symbol_attribute *, sym_intent, locus *);
bool gfc_add_explicit_interface (gfc_symbol *, ifsrc,
				gfc_formal_arglist *, locus *);
bool gfc_add_type (gfc_symbol *, gfc_typespec *, locus *);

void gfc_clear_attr (symbol_attribute *);
bool gfc_missing_attr (symbol_attribute *, locus *);
bool gfc_copy_attr (symbol_attribute *, symbol_attribute *, locus *);
int gfc_copy_dummy_sym (gfc_symbol **, gfc_symbol *, int);
bool gfc_add_component (gfc_symbol *, const char *, gfc_component **);
gfc_symbol *gfc_use_derived (gfc_symbol *);
gfc_symtree *gfc_use_derived_tree (gfc_symtree *);
gfc_component *gfc_find_component (gfc_symbol *, const char *, bool, bool,
                                   gfc_ref **);

gfc_st_label *gfc_get_st_label (int);
void gfc_free_st_label (gfc_st_label *);
void gfc_define_st_label (gfc_st_label *, gfc_sl_type, locus *);
bool gfc_reference_st_label (gfc_st_label *, gfc_sl_type);

gfc_namespace *gfc_get_namespace (gfc_namespace *, int);
gfc_symtree *gfc_new_symtree (gfc_symtree **, const char *);
gfc_symtree *gfc_find_symtree (gfc_symtree *, const char *);
void gfc_delete_symtree (gfc_symtree **, const char *);
gfc_symtree *gfc_get_unique_symtree (gfc_namespace *);
gfc_user_op *gfc_get_uop (const char *);
gfc_user_op *gfc_find_uop (const char *, gfc_namespace *);
void gfc_free_symbol (gfc_symbol *);
void gfc_release_symbol (gfc_symbol *);
gfc_symbol *gfc_new_symbol (const char *, gfc_namespace *);
gfc_symtree* gfc_find_symtree_in_proc (const char *, gfc_namespace *);
int gfc_find_symbol (const char *, gfc_namespace *, int, gfc_symbol **);
int gfc_find_sym_tree (const char *, gfc_namespace *, int, gfc_symtree **);
int gfc_get_symbol (const char *, gfc_namespace *, gfc_symbol **);
bool gfc_verify_c_interop (gfc_typespec *);
bool gfc_verify_c_interop_param (gfc_symbol *);
bool verify_bind_c_sym (gfc_symbol *, gfc_typespec *, int, gfc_common_head *);
bool verify_bind_c_derived_type (gfc_symbol *);
bool verify_com_block_vars_c_interop (gfc_common_head *);
gfc_symtree *generate_isocbinding_symbol (const char *, iso_c_binding_symbol,
					  const char *, gfc_symtree *, bool);
void gfc_save_symbol_data (gfc_symbol *);
int gfc_get_sym_tree (const char *, gfc_namespace *, gfc_symtree **, bool);
int gfc_get_ha_symbol (const char *, gfc_symbol **);
int gfc_get_ha_sym_tree (const char *, gfc_symtree **);

void gfc_new_undo_checkpoint (gfc_undo_change_set &);
void gfc_drop_last_undo_checkpoint (void);
void gfc_restore_last_undo_checkpoint (void);
void gfc_undo_symbols (void);
void gfc_commit_symbols (void);
void gfc_commit_symbol (gfc_symbol *);
gfc_charlen *gfc_new_charlen (gfc_namespace *, gfc_charlen *);
void gfc_free_charlen (gfc_charlen *, gfc_charlen *);
void gfc_free_namespace (gfc_namespace *);

void gfc_symbol_init_2 (void);
void gfc_symbol_done_2 (void);

void gfc_traverse_symtree (gfc_symtree *, void (*)(gfc_symtree *));
void gfc_traverse_ns (gfc_namespace *, void (*)(gfc_symbol *));
void gfc_traverse_user_op (gfc_namespace *, void (*)(gfc_user_op *));
void gfc_save_all (gfc_namespace *);

void gfc_enforce_clean_symbol_state (void);
void gfc_free_dt_list (void);


gfc_gsymbol *gfc_get_gsymbol (const char *);
gfc_gsymbol *gfc_find_gsymbol (gfc_gsymbol *, const char *);
gfc_gsymbol *gfc_find_case_gsymbol (gfc_gsymbol *, const char *);

gfc_typebound_proc* gfc_get_typebound_proc (gfc_typebound_proc*);
gfc_symbol* gfc_get_derived_super_type (gfc_symbol*);
gfc_symbol* gfc_get_ultimate_derived_super_type (gfc_symbol*);
bool gfc_type_is_extension_of (gfc_symbol *, gfc_symbol *);
bool gfc_type_compatible (gfc_typespec *, gfc_typespec *);

void gfc_copy_formal_args_intr (gfc_symbol *, gfc_intrinsic_sym *,
				gfc_actual_arglist *);

void gfc_free_finalizer (gfc_finalizer *el); /* Needed in resolve.c, too  */

bool gfc_check_symbol_typed (gfc_symbol*, gfc_namespace*, bool, locus);
gfc_namespace* gfc_find_proc_namespace (gfc_namespace*);

bool gfc_is_associate_pointer (gfc_symbol*);
gfc_symbol * gfc_find_dt_in_generic (gfc_symbol *);
gfc_formal_arglist *gfc_sym_get_dummy_args (gfc_symbol *);

/* intrinsic.c -- true if working in an init-expr, false otherwise.  */
extern bool gfc_init_expr_flag;

/* Given a symbol that we have decided is intrinsic, mark it as such
   by placing it into a special module that is otherwise impossible to
   read or write.  */

#define gfc_intrinsic_symbol(SYM) SYM->module = gfc_get_string ("(intrinsic)")

void gfc_intrinsic_init_1 (void);
void gfc_intrinsic_done_1 (void);

char gfc_type_letter (bt);
gfc_symbol * gfc_get_intrinsic_sub_symbol (const char *);
bool gfc_convert_type (gfc_expr *, gfc_typespec *, int);
bool gfc_convert_type_warn (gfc_expr *, gfc_typespec *, int, int);
bool gfc_convert_chartype (gfc_expr *, gfc_typespec *);
int gfc_generic_intrinsic (const char *);
int gfc_specific_intrinsic (const char *);
bool gfc_is_intrinsic (gfc_symbol*, int, locus);
int gfc_intrinsic_actual_ok (const char *, const bool);
gfc_intrinsic_sym *gfc_find_function (const char *);
gfc_intrinsic_sym *gfc_find_subroutine (const char *);
gfc_intrinsic_sym *gfc_intrinsic_function_by_id (gfc_isym_id);
gfc_intrinsic_sym *gfc_intrinsic_subroutine_by_id (gfc_isym_id);
gfc_isym_id gfc_isym_id_by_intmod (intmod_id, int);
gfc_isym_id gfc_isym_id_by_intmod_sym (gfc_symbol *);


match gfc_intrinsic_func_interface (gfc_expr *, int);
match gfc_intrinsic_sub_interface (gfc_code *, int);

void gfc_warn_intrinsic_shadow (const gfc_symbol*, bool, bool);
bool gfc_check_intrinsic_standard (const gfc_intrinsic_sym*, const char**,
				      bool, locus);

/* match.c -- FIXME */
void gfc_free_iterator (gfc_iterator *, int);
void gfc_free_forall_iterator (gfc_forall_iterator *);
void gfc_free_alloc_list (gfc_alloc *);
void gfc_free_namelist (gfc_namelist *);
void gfc_free_omp_namelist (gfc_omp_namelist *);
void gfc_free_equiv (gfc_equiv *);
void gfc_free_equiv_until (gfc_equiv *, gfc_equiv *);
void gfc_free_data (gfc_data *);
void gfc_reject_data (gfc_namespace *);
void gfc_free_case_list (gfc_case *);

/* matchexp.c -- FIXME too?  */
gfc_expr *gfc_get_parentheses (gfc_expr *);

/* openmp.c */
struct gfc_omp_saved_state { void *ptrs[2]; int ints[1]; };
void gfc_free_omp_clauses (gfc_omp_clauses *);
void gfc_free_oacc_declare_clauses (struct gfc_oacc_declare *);
void gfc_free_omp_declare_simd (gfc_omp_declare_simd *);
void gfc_free_omp_declare_simd_list (gfc_omp_declare_simd *);
void gfc_free_omp_udr (gfc_omp_udr *);
gfc_omp_udr *gfc_omp_udr_find (gfc_symtree *, gfc_typespec *);
void gfc_resolve_omp_directive (gfc_code *, gfc_namespace *);
void gfc_resolve_do_iterator (gfc_code *, gfc_symbol *, bool);
void gfc_resolve_omp_local_vars (gfc_namespace *);
void gfc_resolve_omp_parallel_blocks (gfc_code *, gfc_namespace *);
void gfc_resolve_omp_do_blocks (gfc_code *, gfc_namespace *);
void gfc_resolve_omp_declare_simd (gfc_namespace *);
void gfc_resolve_omp_udrs (gfc_symtree *);
void gfc_omp_save_and_clear_state (struct gfc_omp_saved_state *);
void gfc_omp_restore_state (struct gfc_omp_saved_state *);
void gfc_free_expr_list (gfc_expr_list *);
void gfc_resolve_oacc_directive (gfc_code *, gfc_namespace *);
void gfc_resolve_oacc_declare (gfc_namespace *);
void gfc_resolve_oacc_parallel_loop_blocks (gfc_code *, gfc_namespace *);
void gfc_resolve_oacc_blocks (gfc_code *, gfc_namespace *);

/* expr.c */
void gfc_free_actual_arglist (gfc_actual_arglist *);
gfc_actual_arglist *gfc_copy_actual_arglist (gfc_actual_arglist *);

bool gfc_extract_int (gfc_expr *, int *, int = 0);
bool gfc_extract_hwi (gfc_expr *, HOST_WIDE_INT *, int = 0);

bool is_subref_array (gfc_expr *);
bool gfc_is_simply_contiguous (gfc_expr *, bool, bool);
bool gfc_check_init_expr (gfc_expr *);

gfc_expr *gfc_build_conversion (gfc_expr *);
void gfc_free_ref_list (gfc_ref *);
void gfc_type_convert_binary (gfc_expr *, int);
bool gfc_is_constant_expr (gfc_expr *);
bool gfc_simplify_expr (gfc_expr *, int);
int gfc_has_vector_index (gfc_expr *);

gfc_expr *gfc_get_expr (void);
gfc_expr *gfc_get_array_expr (bt type, int kind, locus *);
gfc_expr *gfc_get_null_expr (locus *);
gfc_expr *gfc_get_operator_expr (locus *, gfc_intrinsic_op,gfc_expr *, gfc_expr *);
gfc_expr *gfc_get_structure_constructor_expr (bt, int, locus *);
gfc_expr *gfc_get_constant_expr (bt, int, locus *);
gfc_expr *gfc_get_character_expr (int, locus *, const char *, gfc_charlen_t len);
gfc_expr *gfc_get_int_expr (int, locus *, HOST_WIDE_INT);
gfc_expr *gfc_get_logical_expr (int, locus *, bool);
gfc_expr *gfc_get_iokind_expr (locus *, io_kind);

void gfc_clear_shape (mpz_t *shape, int rank);
void gfc_free_shape (mpz_t **shape, int rank);
void gfc_free_expr (gfc_expr *);
void gfc_replace_expr (gfc_expr *, gfc_expr *);
mpz_t *gfc_copy_shape (mpz_t *, int);
mpz_t *gfc_copy_shape_excluding (mpz_t *, int, gfc_expr *);
gfc_expr *gfc_copy_expr (gfc_expr *);
gfc_ref* gfc_copy_ref (gfc_ref*);

bool gfc_specification_expr (gfc_expr *);

int gfc_numeric_ts (gfc_typespec *);
int gfc_kind_max (gfc_expr *, gfc_expr *);

bool gfc_check_conformance (gfc_expr *, gfc_expr *, const char *, ...) ATTRIBUTE_PRINTF_3;
bool gfc_check_assign (gfc_expr *, gfc_expr *, int, bool c = true);
bool gfc_check_pointer_assign (gfc_expr *, gfc_expr *);
bool gfc_check_assign_symbol (gfc_symbol *, gfc_component *, gfc_expr *);

gfc_expr *gfc_build_default_init_expr (gfc_typespec *, locus *);
gfc_expr *gfc_build_init_expr (gfc_typespec *, locus *, bool);
void gfc_apply_init (gfc_typespec *, symbol_attribute *, gfc_expr *);
bool gfc_has_default_initializer (gfc_symbol *);
gfc_expr *gfc_default_initializer (gfc_typespec *);
gfc_expr *gfc_generate_initializer (gfc_typespec *, bool);
gfc_expr *gfc_get_variable_expr (gfc_symtree *);
void gfc_add_full_array_ref (gfc_expr *, gfc_array_spec *);
gfc_expr * gfc_lval_expr_from_sym (gfc_symbol *);

gfc_array_spec *gfc_get_full_arrayspec_from_expr (gfc_expr *expr);

bool gfc_traverse_expr (gfc_expr *, gfc_symbol *,
			bool (*)(gfc_expr *, gfc_symbol *, int*),
			int);
void gfc_expr_set_symbols_referenced (gfc_expr *);
bool gfc_expr_check_typed (gfc_expr*, gfc_namespace*, bool);
bool gfc_derived_parameter_expr (gfc_expr *);
gfc_param_spec_type gfc_spec_list_type (gfc_actual_arglist *, gfc_symbol *);
gfc_component * gfc_get_proc_ptr_comp (gfc_expr *);
bool gfc_is_proc_ptr_comp (gfc_expr *);
bool gfc_is_alloc_class_scalar_function (gfc_expr *);
bool gfc_is_class_array_function (gfc_expr *);

bool gfc_ref_this_image (gfc_ref *ref);
bool gfc_is_coindexed (gfc_expr *);
bool gfc_is_coarray (gfc_expr *);
int gfc_get_corank (gfc_expr *);
bool gfc_has_ultimate_allocatable (gfc_expr *);
bool gfc_has_ultimate_pointer (gfc_expr *);
gfc_expr* gfc_find_team_co (gfc_expr *);
gfc_expr* gfc_find_stat_co (gfc_expr *);
gfc_expr* gfc_build_intrinsic_call (gfc_namespace *, gfc_isym_id, const char*,
				    locus, unsigned, ...);
bool gfc_check_vardef_context (gfc_expr*, bool, bool, bool, const char*);


/* st.c */
extern gfc_code new_st;

void gfc_clear_new_st (void);
gfc_code *gfc_get_code (gfc_exec_op);
gfc_code *gfc_append_code (gfc_code *, gfc_code *);
void gfc_free_statement (gfc_code *);
void gfc_free_statements (gfc_code *);
void gfc_free_association_list (gfc_association_list *);

/* resolve.c */
bool gfc_resolve_expr (gfc_expr *);
void gfc_resolve (gfc_namespace *);
void gfc_resolve_code (gfc_code *, gfc_namespace *);
void gfc_resolve_blocks (gfc_code *, gfc_namespace *);
int gfc_impure_variable (gfc_symbol *);
int gfc_pure (gfc_symbol *);
int gfc_implicit_pure (gfc_symbol *);
void gfc_unset_implicit_pure (gfc_symbol *);
int gfc_elemental (gfc_symbol *);
bool gfc_resolve_iterator (gfc_iterator *, bool, bool);
bool find_forall_index (gfc_expr *, gfc_symbol *, int);
bool gfc_resolve_index (gfc_expr *, int);
bool gfc_resolve_dim_arg (gfc_expr *);
bool gfc_is_formal_arg (void);
void gfc_resolve_substring_charlen (gfc_expr *);
match gfc_iso_c_sub_interface(gfc_code *, gfc_symbol *);
gfc_expr *gfc_expr_to_initialize (gfc_expr *);
bool gfc_type_is_extensible (gfc_symbol *);
bool gfc_resolve_intrinsic (gfc_symbol *, locus *);
bool gfc_explicit_interface_required (gfc_symbol *, char *, int);
extern int gfc_do_concurrent_flag;
const char* gfc_lookup_function_fuzzy (const char *, gfc_symtree *);


/* array.c */
gfc_iterator *gfc_copy_iterator (gfc_iterator *);

void gfc_free_array_spec (gfc_array_spec *);
gfc_array_ref *gfc_copy_array_ref (gfc_array_ref *);

bool gfc_set_array_spec (gfc_symbol *, gfc_array_spec *, locus *);
gfc_array_spec *gfc_copy_array_spec (gfc_array_spec *);
bool gfc_resolve_array_spec (gfc_array_spec *, int);

int gfc_compare_array_spec (gfc_array_spec *, gfc_array_spec *);

void gfc_simplify_iterator_var (gfc_expr *);
bool gfc_expand_constructor (gfc_expr *, bool);
int gfc_constant_ac (gfc_expr *);
int gfc_expanded_ac (gfc_expr *);
bool gfc_resolve_character_array_constructor (gfc_expr *);
bool gfc_resolve_array_constructor (gfc_expr *);
bool gfc_check_constructor_type (gfc_expr *);
bool gfc_check_iter_variable (gfc_expr *);
bool gfc_check_constructor (gfc_expr *, bool (*)(gfc_expr *));
bool gfc_array_size (gfc_expr *, mpz_t *);
bool gfc_array_dimen_size (gfc_expr *, int, mpz_t *);
bool gfc_array_ref_shape (gfc_array_ref *, mpz_t *);
gfc_array_ref *gfc_find_array_ref (gfc_expr *, bool a = false);
tree gfc_conv_array_initializer (tree type, gfc_expr *);
bool spec_size (gfc_array_spec *, mpz_t *);
bool spec_dimen_size (gfc_array_spec *, int, mpz_t *);
bool gfc_is_compile_time_shape (gfc_array_spec *);

bool gfc_ref_dimen_size (gfc_array_ref *, int dimen, mpz_t *, mpz_t *);


/* interface.c -- FIXME: some of these should be in symbol.c */
void gfc_free_interface (gfc_interface *);
bool gfc_compare_derived_types (gfc_symbol *, gfc_symbol *);
bool gfc_compare_types (gfc_typespec *, gfc_typespec *);
bool gfc_check_dummy_characteristics (gfc_symbol *, gfc_symbol *,
				      bool, char *, int);
bool gfc_check_result_characteristics (gfc_symbol *, gfc_symbol *,
				       char *, int);
bool gfc_compare_interfaces (gfc_symbol*, gfc_symbol*, const char *, int, int,
			     char *, int, const char *, const char *);
void gfc_check_interfaces (gfc_namespace *);
bool gfc_procedure_use (gfc_symbol *, gfc_actual_arglist **, locus *);
void gfc_ppc_use (gfc_component *, gfc_actual_arglist **, locus *);
gfc_symbol *gfc_search_interface (gfc_interface *, int,
				  gfc_actual_arglist **);
match gfc_extend_expr (gfc_expr *);
void gfc_free_formal_arglist (gfc_formal_arglist *);
bool gfc_extend_assign (gfc_code *, gfc_namespace *);
bool gfc_check_new_interface (gfc_interface *, gfc_symbol *, locus);
bool gfc_add_interface (gfc_symbol *);
gfc_interface *gfc_current_interface_head (void);
void gfc_set_current_interface_head (gfc_interface *);
gfc_symtree* gfc_find_sym_in_symtree (gfc_symbol*);
bool gfc_arglist_matches_symbol (gfc_actual_arglist**, gfc_symbol*);
bool gfc_check_operator_interface (gfc_symbol*, gfc_intrinsic_op, locus);
bool gfc_has_vector_subscript (gfc_expr*);
gfc_intrinsic_op gfc_equivalent_op (gfc_intrinsic_op);
bool gfc_check_typebound_override (gfc_symtree*, gfc_symtree*);
void gfc_check_dtio_interfaces (gfc_symbol*);
gfc_symtree* gfc_find_typebound_dtio_proc (gfc_symbol *, bool, bool);
gfc_symbol* gfc_find_specific_dtio_proc (gfc_symbol*, bool, bool);


/* io.c */
extern gfc_st_label format_asterisk;

void gfc_free_open (gfc_open *);
bool gfc_resolve_open (gfc_open *);
void gfc_free_close (gfc_close *);
bool gfc_resolve_close (gfc_close *);
void gfc_free_filepos (gfc_filepos *);
bool gfc_resolve_filepos (gfc_filepos *);
void gfc_free_inquire (gfc_inquire *);
bool gfc_resolve_inquire (gfc_inquire *);
void gfc_free_dt (gfc_dt *);
bool gfc_resolve_dt (gfc_dt *, locus *);
void gfc_free_wait (gfc_wait *);
bool gfc_resolve_wait (gfc_wait *);
extern bool async_io_dt;

/* module.c */
void gfc_module_init_2 (void);
void gfc_module_done_2 (void);
void gfc_dump_module (const char *, int);
bool gfc_check_symbol_access (gfc_symbol *);
void gfc_free_use_stmts (gfc_use_list *);
const char *gfc_dt_lower_string (const char *);
const char *gfc_dt_upper_string (const char *);

/* primary.c */
symbol_attribute gfc_variable_attr (gfc_expr *, gfc_typespec *);
symbol_attribute gfc_expr_attr (gfc_expr *);
symbol_attribute gfc_caf_attr (gfc_expr *, bool i = false, bool *r = NULL);
match gfc_match_rvalue (gfc_expr **);
match gfc_match_varspec (gfc_expr*, int, bool, bool);
int gfc_check_digit (char, int);
bool gfc_is_function_return_value (gfc_symbol *, gfc_namespace *);
bool gfc_convert_to_structure_constructor (gfc_expr *, gfc_symbol *,
					      gfc_expr **,
					      gfc_actual_arglist **, bool);

/* trans.c */
void gfc_generate_code (gfc_namespace *);
void gfc_generate_module_code (gfc_namespace *);

/* trans-intrinsic.c */
bool gfc_inline_intrinsic_function_p (gfc_expr *);

/* bbt.c */
typedef int (*compare_fn) (void *, void *);
void gfc_insert_bbt (void *, void *, compare_fn);
void gfc_delete_bbt (void *, void *, compare_fn);

/* dump-parse-tree.c */
void gfc_dump_parse_tree (gfc_namespace *, FILE *);
void gfc_dump_c_prototypes (gfc_namespace *, FILE *);

/* parse.c */
bool gfc_parse_file (void);
void gfc_global_used (gfc_gsymbol *, locus *);
gfc_namespace* gfc_build_block_ns (gfc_namespace *);

/* dependency.c */
int gfc_dep_compare_functions (gfc_expr *, gfc_expr *, bool);
int gfc_dep_compare_expr (gfc_expr *, gfc_expr *);
bool gfc_dep_difference (gfc_expr *, gfc_expr *, mpz_t *);

/* check.c */
bool gfc_check_same_strlen (const gfc_expr*, const gfc_expr*, const char*);
bool gfc_calculate_transfer_sizes (gfc_expr*, gfc_expr*, gfc_expr*,
				      size_t*, size_t*, size_t*);

/* class.c */
void gfc_fix_class_refs (gfc_expr *e);
void gfc_add_component_ref (gfc_expr *, const char *);
void gfc_add_class_array_ref (gfc_expr *);
#define gfc_add_data_component(e)     gfc_add_component_ref(e,"_data")
#define gfc_add_vptr_component(e)     gfc_add_component_ref(e,"_vptr")
#define gfc_add_len_component(e)      gfc_add_component_ref(e,"_len")
#define gfc_add_hash_component(e)     gfc_add_component_ref(e,"_hash")
#define gfc_add_size_component(e)     gfc_add_component_ref(e,"_size")
#define gfc_add_def_init_component(e) gfc_add_component_ref(e,"_def_init")
#define gfc_add_final_component(e)    gfc_add_component_ref(e,"_final")
bool gfc_is_class_array_ref (gfc_expr *, bool *);
bool gfc_is_class_scalar_expr (gfc_expr *);
bool gfc_is_class_container_ref (gfc_expr *e);
gfc_expr *gfc_class_initializer (gfc_typespec *, gfc_expr *);
unsigned int gfc_hash_value (gfc_symbol *);
gfc_expr *gfc_get_len_component (gfc_expr *e);
bool gfc_build_class_symbol (gfc_typespec *, symbol_attribute *,
			     gfc_array_spec **);
gfc_symbol *gfc_find_derived_vtab (gfc_symbol *);
gfc_symbol *gfc_find_vtab (gfc_typespec *);
gfc_symtree* gfc_find_typebound_proc (gfc_symbol*, bool*,
				      const char*, bool, locus*);
gfc_symtree* gfc_find_typebound_user_op (gfc_symbol*, bool*,
					 const char*, bool, locus*);
gfc_typebound_proc* gfc_find_typebound_intrinsic_op (gfc_symbol*, bool*,
						     gfc_intrinsic_op, bool,
						     locus*);
gfc_symtree* gfc_get_tbp_symtree (gfc_symtree**, const char*);
bool gfc_is_finalizable (gfc_symbol *, gfc_expr **);

#define CLASS_DATA(sym) sym->ts.u.derived->components
#define UNLIMITED_POLY(sym) \
	(sym != NULL && sym->ts.type == BT_CLASS \
	 && CLASS_DATA (sym) \
	 && CLASS_DATA (sym)->ts.u.derived \
	 && CLASS_DATA (sym)->ts.u.derived->attr.unlimited_polymorphic)
#define IS_CLASS_ARRAY(sym) \
	(sym->ts.type == BT_CLASS \
	 && CLASS_DATA (sym) \
	 && CLASS_DATA (sym)->attr.dimension \
	 && !CLASS_DATA (sym)->attr.class_pointer)

/* frontend-passes.c */

void gfc_run_passes (gfc_namespace *);

typedef int (*walk_code_fn_t) (gfc_code **, int *, void *);
typedef int (*walk_expr_fn_t) (gfc_expr **, int *, void *);

int gfc_dummy_code_callback (gfc_code **, int *, void *);
int gfc_expr_walker (gfc_expr **, walk_expr_fn_t, void *);
int gfc_code_walker (gfc_code **, walk_code_fn_t, walk_expr_fn_t, void *);

/* simplify.c */

void gfc_convert_mpz_to_signed (mpz_t, int);
gfc_expr *gfc_simplify_ieee_functions (gfc_expr *);
bool gfc_is_size_zero_array (gfc_expr *);

/* trans-array.c  */

bool gfc_is_reallocatable_lhs (gfc_expr *);

/* trans-decl.c */

void finish_oacc_declare (gfc_namespace *, gfc_symbol *, bool);

#endif /* GCC_GFORTRAN_H  */
