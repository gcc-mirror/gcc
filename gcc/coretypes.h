/* GCC core type declarations.
   Copyright (C) 2002-2015 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Provide forward declarations of core types which are referred to by
   most of the compiler.  This allows header files to use these types
   (e.g. in function prototypes) without concern for whether the full
   definitions are visible.  Some other declarations that need to be
   universally visible are here, too.

   In the context of tconfig.h, most of these have special definitions
   which prevent them from being used except in further type
   declarations.  This is a kludge; the right thing is to avoid
   including the "tm.h" header set in the context of tconfig.h, but
   we're not there yet.  */

#ifndef GCC_CORETYPES_H
#define GCC_CORETYPES_H

#ifndef GTY
#define GTY(x)  /* nothing - marker for gengtype */
#endif

#ifndef USED_FOR_TARGET

typedef int64_t gcov_type;
typedef uint64_t gcov_type_unsigned;

struct bitmap_head;
typedef struct bitmap_head *bitmap;
typedef const struct bitmap_head *const_bitmap;
struct simple_bitmap_def;
typedef struct simple_bitmap_def *sbitmap;
typedef const struct simple_bitmap_def *const_sbitmap;
struct rtx_def;
typedef struct rtx_def *rtx;
typedef const struct rtx_def *const_rtx;

/* Subclasses of rtx_def, using indentation to show the class
   hierarchy, along with the relevant invariant.
   Where possible, keep this list in the same order as in rtl.def.  */
class rtx_def;
  class rtx_expr_list;           /* GET_CODE (X) == EXPR_LIST */
  class rtx_insn_list;           /* GET_CODE (X) == INSN_LIST */
  class rtx_sequence;            /* GET_CODE (X) == SEQUENCE */
  class rtx_insn;
    class rtx_debug_insn;      /* DEBUG_INSN_P (X) */
    class rtx_nonjump_insn;    /* NONJUMP_INSN_P (X) */
    class rtx_jump_insn;       /* JUMP_P (X) */
    class rtx_call_insn;       /* CALL_P (X) */
    class rtx_jump_table_data; /* JUMP_TABLE_DATA_P (X) */
    class rtx_barrier;         /* BARRIER_P (X) */
    class rtx_code_label;      /* LABEL_P (X) */
    class rtx_note;            /* NOTE_P (X) */

struct rtvec_def;
typedef struct rtvec_def *rtvec;
typedef const struct rtvec_def *const_rtvec;
struct hwivec_def;
typedef struct hwivec_def *hwivec;
typedef const struct hwivec_def *const_hwivec;
union tree_node;
typedef union tree_node *tree;
typedef const union tree_node *const_tree;
typedef struct gimple_statement_base *gimple;
typedef const struct gimple_statement_base *const_gimple;
typedef gimple gimple_seq;
struct gimple_stmt_iterator;

/* Forward decls for leaf gimple subclasses (for individual gimple codes).
   Keep this in the same order as the corresponding codes in gimple.def.  */

struct gcond;
struct gdebug;
struct ggoto;
struct glabel;
struct gswitch;
struct gassign;
struct gasm;
struct gcall;
struct gtransaction;
struct greturn;
struct gbind;
struct gcatch;
struct geh_filter;
struct geh_mnt;
struct geh_else;
struct gresx;
struct geh_dispatch;
struct gphi;
struct gtry;
struct gomp_atomic_load;
struct gomp_atomic_store;
struct gomp_continue;
struct gomp_critical;
struct gomp_for;
struct gomp_parallel;
struct gomp_task;
struct gomp_sections;
struct gomp_single;
struct gomp_target;
struct gomp_teams;

union section;
typedef union section section;
struct gcc_options;
struct cl_target_option;
struct cl_optimization;
struct cl_option;
struct cl_decoded_option;
struct cl_option_handlers;
struct diagnostic_context;
struct pretty_printer;

/* Address space number for named address space support.  */
typedef unsigned char addr_space_t;

/* The value of addr_space_t that represents the generic address space.  */
#define ADDR_SPACE_GENERIC 0
#define ADDR_SPACE_GENERIC_P(AS) ((AS) == ADDR_SPACE_GENERIC)

/* The major intermediate representations of GCC.  */
enum ir_type {
  IR_GIMPLE,
  IR_RTL_CFGRTL,
  IR_RTL_CFGLAYOUT
};

/* Provide forward struct declaration so that we don't have to include
   all of cpplib.h whenever a random prototype includes a pointer.
   Note that the cpp_reader and cpp_token typedefs remain part of
   cpplib.h.  */

struct cpp_reader;
struct cpp_token;

/* The thread-local storage model associated with a given VAR_DECL
   or SYMBOL_REF.  This isn't used much, but both trees and RTL refer
   to it, so it's here.  */
enum tls_model {
  TLS_MODEL_NONE,
  TLS_MODEL_EMULATED,
  TLS_MODEL_REAL,
  TLS_MODEL_GLOBAL_DYNAMIC = TLS_MODEL_REAL,
  TLS_MODEL_LOCAL_DYNAMIC,
  TLS_MODEL_INITIAL_EXEC,
  TLS_MODEL_LOCAL_EXEC
};

/* Types of ABI for an offload compiler.  */
enum offload_abi {
  OFFLOAD_ABI_UNSET,
  OFFLOAD_ABI_LP64,
  OFFLOAD_ABI_ILP32
};

/* Types of unwind/exception handling info that can be generated.  */

enum unwind_info_type
{
  UI_NONE,
  UI_SJLJ,
  UI_DWARF2,
  UI_TARGET,
  UI_SEH
};

/* Callgraph node profile representation.  */
enum node_frequency {
  /* This function most likely won't be executed at all.
     (set only when profile feedback is available or via function attribute). */
  NODE_FREQUENCY_UNLIKELY_EXECUTED,
  /* For functions that are known to be executed once (i.e. constructors, destructors
     and main function.  */
  NODE_FREQUENCY_EXECUTED_ONCE,
  /* The default value.  */
  NODE_FREQUENCY_NORMAL,
  /* Optimize this function hard
     (set only when profile feedback is available or via function attribute). */
  NODE_FREQUENCY_HOT
};

/* Possible initialization status of a variable.   When requested
   by the user, this information is tracked and recorded in the DWARF
   debug information, along with the variable's location.  */
enum var_init_status
{
  VAR_INIT_STATUS_UNKNOWN,
  VAR_INIT_STATUS_UNINITIALIZED,
  VAR_INIT_STATUS_INITIALIZED
};

/* The type of an alias set.  Code currently assumes that variables of
   this type can take the values 0 (the alias set which aliases
   everything) and -1 (sometimes indicating that the alias set is
   unknown, sometimes indicating a memory barrier) and -2 (indicating
   that the alias set should be set to a unique value but has not been
   set yet).  */
typedef int alias_set_type;

struct edge_def;
typedef struct edge_def *edge;
typedef const struct edge_def *const_edge;
struct basic_block_def;
typedef struct basic_block_def *basic_block;
typedef const struct basic_block_def *const_basic_block;

#define obstack_chunk_alloc	xmalloc
#define obstack_chunk_free	free
#define OBSTACK_CHUNK_SIZE	0
#define gcc_obstack_init(OBSTACK)				\
  obstack_specify_allocation ((OBSTACK), OBSTACK_CHUNK_SIZE, 0,	\
			      obstack_chunk_alloc,		\
			      obstack_chunk_free)

/* enum reg_class is target specific, so it should not appear in
   target-independent code or interfaces, like the target hook declarations
   in target.h.  */
typedef int reg_class_t;

class rtl_opt_pass;

namespace gcc {
  class context;
}

#else

struct _dont_use_rtx_here_;
struct _dont_use_rtvec_here_;
struct _dont_use_rtx_insn_here_;
union _dont_use_tree_here_;
#define rtx struct _dont_use_rtx_here_ *
#define const_rtx struct _dont_use_rtx_here_ *
#define rtvec struct _dont_use_rtvec_here *
#define const_rtvec struct _dont_use_rtvec_here *
#define rtx_insn struct _dont_use_rtx_insn_here_
#define tree union _dont_use_tree_here_ *
#define const_tree union _dont_use_tree_here_ *

#endif

/* Classes of functions that compiler needs to check
   whether they are present at the runtime or not.  */
enum function_class {
  function_c94,
  function_c99_misc,
  function_c99_math_complex,
  function_sincos,
  function_c11_misc
};

/* Suppose that higher bits are target dependent. */
#define MEMMODEL_MASK ((1<<16)-1)

/* Legacy sync operations set this upper flag in the memory model.  This allows
   targets that need to do something stronger for sync operations to
   differentiate with their target patterns and issue a more appropriate insn
   sequence.  See bugzilla 65697 for background.  */
#define MEMMODEL_SYNC (1<<15)

/* Memory model without SYNC bit for targets/operations that do not care.  */
#define MEMMODEL_BASE_MASK (MEMMODEL_SYNC-1)

/* Memory model types for the __atomic* builtins. 
   This must match the order in libstdc++-v3/include/bits/atomic_base.h.  */
enum memmodel
{
  MEMMODEL_RELAXED = 0,
  MEMMODEL_CONSUME = 1,
  MEMMODEL_ACQUIRE = 2,
  MEMMODEL_RELEASE = 3,
  MEMMODEL_ACQ_REL = 4,
  MEMMODEL_SEQ_CST = 5,
  MEMMODEL_LAST = 6,
  MEMMODEL_SYNC_ACQUIRE = MEMMODEL_ACQUIRE | MEMMODEL_SYNC,
  MEMMODEL_SYNC_RELEASE = MEMMODEL_RELEASE | MEMMODEL_SYNC,
  MEMMODEL_SYNC_SEQ_CST = MEMMODEL_SEQ_CST | MEMMODEL_SYNC
};

/* Support for user-provided GGC and PCH markers.  The first parameter
   is a pointer to a pointer, the second a cookie.  */
typedef void (*gt_pointer_operator) (void *, void *);

#if !defined (HAVE_UCHAR)
typedef unsigned char uchar;
#endif

/* Most host source files will require the following headers.  */
#if !defined (GENERATOR_FILE) && !defined (USED_FOR_TARGET)
#include "machmode.h"
#include "signop.h"
#include "wide-int.h" 
#include "double-int.h"
#include "real.h"
#include "fixed-value.h"
#include "hash-table.h"
#include "hash-set.h"
#include "input.h"
#include "is-a.h"
#endif /* GENERATOR_FILE && !USED_FOR_TARGET */

#endif /* coretypes.h */
