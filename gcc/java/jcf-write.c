/* Write out a Java(TM) class file.
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "jcf.h"
#include "tree.h"
#include "java-tree.h"
#include "obstack.h"
#undef AND
#include "rtl.h"
#include "flags.h"
#include "java-opcodes.h"
#include "parse.h" /* for BLOCK_EXPR_BODY */
#include "buffer.h"
#include "toplev.h"

#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '/'
#endif

extern struct obstack temporary_obstack;

/* Base directory in which `.class' files should be written.
   NULL means to put the file into the same directory as the
   corresponding .java file.  */
char *jcf_write_base_directory = NULL;

/* Make sure bytecode.data is big enough for at least N more bytes. */

#define RESERVE(N) \
  do { CHECK_OP(state); \
    if (state->bytecode.ptr + (N) > state->bytecode.limit) \
    buffer_grow (&state->bytecode, N); } while (0)

/* Add a 1-byte instruction/operand I to bytecode.data,
   assuming space has already been RESERVE'd. */

#define OP1(I) (*state->bytecode.ptr++ = (I), CHECK_OP(state))

/* Like OP1, but I is a 2-byte big endian integer. */

#define OP2(I) \
  do { int _i = (I); OP1 (_i >> 8);  OP1 (_i); CHECK_OP(state); } while (0)

/* Like OP1, but I is a 4-byte big endian integer. */

#define OP4(I) \
  do { int _i = (I);  OP1 (_i >> 24);  OP1 (_i >> 16); \
       OP1 (_i >> 8); OP1 (_i); CHECK_OP(state); } while (0)

/* Macro to call each time we push I words on the JVM stack. */

#define NOTE_PUSH(I) \
  do { state->code_SP += (I); \
    if (state->code_SP > state->code_SP_max) \
      state->code_SP_max = state->code_SP; } while (0)

/* Macro to call each time we pop I words from the JVM stack. */

#define NOTE_POP(I) \
  do { state->code_SP -= (I); if (state->code_SP < 0) abort(); } while (0)

/* A chunk or segment of a .class file. */

struct chunk
{
  /* The next segment of this .class file. */
  struct chunk *next;

  /* The actual data in this segment to be written to the .class file. */
  unsigned char *data;

  /* The size of the segment to be written to the .class file. */
  int size;
};

#define PENDING_CLEANUP_PC (-3)
#define PENDING_EXIT_PC (-2)
#define UNDEFINED_PC (-1)

/* Each "block" represents a label plus the bytecode instructions following.
   There may be branches out of the block, but no incoming jumps, except
   to the beginning of the block.

   If (pc < 0), the jcf_block is not an actual block (i.e. it has no
   assocated code yet), but it is an undefined label.
*/

struct jcf_block
{
  /* For blocks that that are defined, the next block (in pc order).
     For blocks that are the not-yet-defined end label of a LABELED_BLOCK_EXPR
     or a cleanup expression (from a WITH_CLEANUP_EXPR),
     this is the next (outer) such end label, in a stack headed by
     labeled_blocks in jcf_partial. */
  struct jcf_block *next;

  /* In the not-yet-defined end label for an unfinished EXIT_BLOCK_EXPR.
     pc is PENDING_EXIT_PC.
     In the not-yet-defined end label for pending cleanup subroutine,
     pc is PENDING_CLEANUP_PC.
     For other not-yet-defined labels, pc is UNDEFINED_PC.

     If the label has been defined:
     Until perform_relocations is finished, this is the maximum possible
     value of the bytecode offset at the begnning of this block.
     After perform_relocations, it is the actual offset (pc). */
  int pc;

  int linenumber;

  /* After finish_jcf_block is called, The actual instructions contained in this block.
     Before than NULL, and the instructions are in state->bytecode. */
  union {
    struct chunk *chunk;

    /* If pc==PENDING_CLEANUP_PC, start_label is the start of the region
       coveed by the cleanup. */
    struct jcf_block *start_label;
  } v;

  union {
    /* Set of relocations (in reverse offset order) for this block. */
    struct jcf_relocation *relocations;

    /* If this block is that of the not-yet-defined end label of
       a LABELED_BLOCK_EXPR, where LABELED_BLOCK is that LABELED_BLOCK_EXPR.
       If pc==PENDING_CLEANUP_PC, the cleanup that needs to be run. */
    tree labeled_block;
  } u;
};

/* A "relocation" type for the 0-3 bytes of padding at the start
   of a tableswitch or a lookupswitch. */
#define SWITCH_ALIGN_RELOC 4

/* A relocation type for the labels in a tableswitch or a lookupswitch;
   these are relative to the start of the instruction, but (due to
   th 0-3 bytes of padding), we don't know the offset before relocation. */
#define BLOCK_START_RELOC 1

struct jcf_relocation
{
  /* Next relocation for the current jcf_block. */
  struct jcf_relocation *next;

  /* The (byte) offset within the current block that needs to be relocated. */
  HOST_WIDE_INT offset;

  /* 0 if offset is a 4-byte relative offset.
     4 (SWITCH_ALIGN_RELOC) if offset points to 0-3 padding bytes inserted
     for proper alignment in tableswitch/lookupswitch instructions.
     1 (BLOCK_START_RELOC) if offset points to a 4-byte offset relative
     to the start of the containing block.
     -1 if offset is a 2-byte relative offset.
     < -1 if offset is the address of an instruction with a 2-byte offset
     that does not have a corresponding 4-byte offset version, in which
     case the absolute value of kind is the inverted opcode.
     > 4 if offset is the address of an instruction (such as jsr) with a
     2-byte offset that does have a corresponding 4-byte offset version,
     in which case kind is the opcode of the 4-byte version (such as jsr_w). */
  int kind;

  /* The label the relocation wants to actually transfer to. */
  struct jcf_block *label;
};

#define RELOCATION_VALUE_0 ((HOST_WIDE_INT)0)
#define RELOCATION_VALUE_1 ((HOST_WIDE_INT)1)

/* State for single catch clause. */

struct jcf_handler
{
  struct jcf_handler *next;

  struct jcf_block *start_label;
  struct jcf_block *end_label;
  struct jcf_block *handler_label;

  /* The sub-class of Throwable handled, or NULL_TREE (for finally). */
  tree type;
};

/* State for the current switch statement. */

struct jcf_switch_state
{
  struct jcf_switch_state *prev;
  struct jcf_block *default_label;

  struct jcf_relocation *cases;
  int num_cases;
  HOST_WIDE_INT min_case, max_case;
};

/* This structure is used to contain the various pieces that will
   become a .class file. */

struct jcf_partial
{
  struct chunk *first;
  struct chunk *chunk;
  struct obstack *chunk_obstack;
  tree current_method;

  /* List of basic blocks for the current method. */
  struct jcf_block *blocks;
  struct jcf_block *last_block;

  struct localvar_info *first_lvar;
  struct localvar_info *last_lvar;
  int lvar_count;

  CPool cpool;

  int linenumber_count;

  /* Until perform_relocations, this is a upper bound on the number
     of bytes (so far) in the instructions for the current method. */
  int code_length;

  /* Stack of undefined ending labels for LABELED_BLOCK_EXPR. */
  struct jcf_block *labeled_blocks;
  
  /* The current stack size (stack pointer) in the current method. */
  int code_SP;

  /* The largest extent of stack size (stack pointer) in the current method. */
  int code_SP_max;

  /* Contains a mapping from local var slot number to localvar_info. */
  struct buffer localvars;

  /* The buffer allocated for bytecode for the current jcf_block. */
  struct buffer bytecode;

  /* Chain of exception handlers for the current method. */
  struct jcf_handler *handlers;

  /* Last element in handlers chain. */
  struct jcf_handler *last_handler;

  /* Number of exception handlers for the current method. */
  int num_handlers;

  /* Number of finalizers we are currently nested within. */
  int num_finalizers;

  /* If non-NULL, use this for the return value. */
  tree return_value_decl;

  /* Information about the current switch statemenet. */
  struct jcf_switch_state *sw_state;
};

static void generate_bytecode_insns PROTO ((tree, int, struct jcf_partial *));
static struct chunk * alloc_chunk PROTO ((struct chunk *, unsigned char *,
					  int, struct obstack *));
static unsigned char * append_chunk PROTO ((unsigned char *, int,
					    struct jcf_partial *));
static void append_chunk_copy PROTO ((unsigned char *, int,
				      struct jcf_partial *));
static struct jcf_block * gen_jcf_label PROTO ((struct jcf_partial *));
static void finish_jcf_block PROTO ((struct jcf_partial *));
static void define_jcf_label PROTO ((struct jcf_block *,
				     struct jcf_partial *));
static struct jcf_block * get_jcf_label_here PROTO ((struct jcf_partial *));
static void put_linenumber PROTO ((int, struct jcf_partial *));
static void localvar_alloc PROTO ((tree, struct jcf_partial *));
static void localvar_free PROTO ((tree, struct jcf_partial *));
static int get_access_flags PROTO ((tree));
static void write_chunks PROTO ((FILE *, struct chunk *));
static int adjust_typed_op PROTO ((tree, int));
static void generate_bytecode_conditional PROTO ((tree, struct jcf_block *,
						  struct jcf_block *, int,
						  struct jcf_partial *));
static void generate_bytecode_return PROTO ((tree, struct jcf_partial *));
static void perform_relocations PROTO ((struct jcf_partial *));
static void init_jcf_state PROTO ((struct jcf_partial *, struct obstack *));
static void init_jcf_method PROTO ((struct jcf_partial *, tree));
static void release_jcf_state PROTO ((struct jcf_partial *));
static struct chunk * generate_classfile PROTO ((tree, struct jcf_partial *));
static struct jcf_handler *alloc_handler PROTO ((struct jcf_block *,
						 struct jcf_block *,
						 struct jcf_partial *));
static void emit_iinc PROTO ((tree, HOST_WIDE_INT, struct jcf_partial *));
static void emit_reloc PROTO ((HOST_WIDE_INT, int, struct jcf_block *, 
			       struct jcf_partial *));
static void push_constant1 PROTO ((HOST_WIDE_INT, struct jcf_partial *));
static void push_constant2 PROTO ((HOST_WIDE_INT, struct jcf_partial *));
static void push_int_const PROTO ((HOST_WIDE_INT, struct jcf_partial *));
static int find_constant_wide PROTO ((HOST_WIDE_INT, HOST_WIDE_INT,
				      struct jcf_partial *));
static void push_long_const PROTO ((HOST_WIDE_INT, HOST_WIDE_INT, 
				    struct jcf_partial *));
static int find_constant_index PROTO ((tree, struct jcf_partial *));
static void push_long_const PROTO ((HOST_WIDE_INT, HOST_WIDE_INT,
				    struct jcf_partial *));
static void field_op PROTO ((tree, int, struct jcf_partial *));
static void maybe_wide PROTO ((int, int, struct jcf_partial *));
static void emit_dup PROTO ((int, int, struct jcf_partial *));
static void emit_pop PROTO ((int, struct jcf_partial *));
static void emit_load_or_store PROTO ((tree, int, struct jcf_partial *));
static void emit_load PROTO ((tree, struct jcf_partial *));
static void emit_store PROTO ((tree, struct jcf_partial *));
static void emit_unop PROTO ((enum java_opcode, tree, struct jcf_partial *));
static void emit_binop PROTO ((enum java_opcode, tree, struct jcf_partial *));
static void emit_reloc PROTO ((HOST_WIDE_INT, int, struct jcf_block *,
			       struct jcf_partial *));
static void emit_switch_reloc PROTO ((struct jcf_block *,
				      struct jcf_partial *));
static void emit_case_reloc PROTO ((struct jcf_relocation *,
				    struct jcf_partial *));
static void emit_if PROTO ((struct jcf_block *, int, int,
			    struct jcf_partial *));
static void emit_goto PROTO ((struct jcf_block *, struct jcf_partial *));
static void emit_jsr PROTO ((struct jcf_block *, struct jcf_partial *));
static void call_cleanups PROTO ((struct jcf_block *, struct jcf_partial *));
static char *make_class_file_name PROTO ((tree));

/* Utility macros for appending (big-endian) data to a buffer.
   We assume a local variable 'ptr' points into where we want to
   write next, and we assume enoygh space has been allocated. */

#ifdef ENABLE_CHECKING
int
CHECK_PUT(ptr, state, i)
     void *ptr;
     struct jcf_partial *state;
     int i;
{
  if (ptr < state->chunk->data
      || (char*)ptr + i > state->chunk->data + state->chunk->size)
    fatal ("internal error - CHECK_PUT failed");
  return 0;
}
#else
#define CHECK_PUT(PTR, STATE, I) ((void)0)
#endif

#define PUT1(X)  (CHECK_PUT(ptr, state, 1), *ptr++ = (X))
#define PUT2(X)  (PUT1((X) >> 8), PUT1((X) & 0xFF))
#define PUT4(X)  (PUT2((X) >> 16), PUT2((X) & 0xFFFF))
#define PUTN(P, N)  (CHECK_PUT(ptr, state, N), memcpy(ptr, P, N), ptr += (N))

/* There are some cases below where CHECK_PUT is guaranteed to fail.
   Use the following macros in those specific cases.  */
#define UNSAFE_PUT1(X)  (*ptr++ = (X))
#define UNSAFE_PUT2(X)  (UNSAFE_PUT1((X) >> 8), UNSAFE_PUT1((X) & 0xFF))
#define UNSAFE_PUT4(X)  (UNSAFE_PUT2((X) >> 16), UNSAFE_PUT2((X) & 0xFFFF))
#define UNSAFE_PUTN(P, N)  (memcpy(ptr, P, N), ptr += (N))


/* Allocate a new chunk on obstack WORK, and link it in after LAST.
   Set the data and size fields to DATA and SIZE, respectively.
   However, if DATA is NULL and SIZE>0, allocate a buffer as well. */

static struct chunk *
alloc_chunk (last, data, size, work)
     struct chunk *last;
     unsigned char *data;
     int size;
     struct obstack *work;
{
  struct chunk *chunk = (struct chunk *)
    obstack_alloc (work, sizeof(struct chunk));

  if (data == NULL && size > 0)
    data = obstack_alloc (work, size);

  chunk->next = NULL;
  chunk->data = data;
  chunk->size = size;
  if (last != NULL)
    last->next = chunk;
  return chunk;
}

#ifdef ENABLE_CHECKING
int
CHECK_OP(struct jcf_partial *state)
{
  if (state->bytecode.ptr > state->bytecode.limit)
    {
      fatal("internal error - CHECK_OP failed");
    }
  return 0;
}
#else
#define CHECK_OP(STATE) ((void)0)
#endif

static unsigned char *
append_chunk (data, size, state)
     unsigned char *data;
     int size;
     struct jcf_partial *state;
{
  state->chunk = alloc_chunk (state->chunk, data, size, state->chunk_obstack);
  if (state->first == NULL)
    state->first = state->chunk;
  return state->chunk->data;
}

static void
append_chunk_copy (data, size, state)
     unsigned char *data;
     int size;
     struct jcf_partial *state;
{
  unsigned char *ptr = append_chunk (NULL, size, state);
  memcpy (ptr, data, size);
}

static struct jcf_block *
gen_jcf_label (state)
     struct jcf_partial *state;
{
  struct jcf_block *block = (struct jcf_block *)
    obstack_alloc (state->chunk_obstack, sizeof (struct jcf_block));
  block->next =	NULL;
  block->linenumber = -1;
  block->pc = UNDEFINED_PC;
  return block;
}

static void
finish_jcf_block (state)
     struct jcf_partial *state;
{
  struct jcf_block *block = state->last_block;
  struct jcf_relocation *reloc;
  int code_length = BUFFER_LENGTH (&state->bytecode);
  int pc = state->code_length;
  append_chunk_copy (state->bytecode.data, code_length, state);
  BUFFER_RESET (&state->bytecode);
  block->v.chunk = state->chunk;

  /* Calculate code_length to the maximum value it can have. */
  pc += block->v.chunk->size;
  for (reloc = block->u.relocations;  reloc != NULL;  reloc = reloc->next)
    {
      int kind = reloc->kind;
      if (kind == SWITCH_ALIGN_RELOC)
	pc += 3;
      else if (kind > BLOCK_START_RELOC)
	pc += 2; /* 2-byte offset may grow to 4-byte offset */
      else if (kind < -1)
	pc += 5; /* May need to add a goto_w. */
    }
  state->code_length = pc;
}

static void
define_jcf_label (label, state)
     struct jcf_block *label;
     struct jcf_partial *state;
{
  if (state->last_block != NULL)
    finish_jcf_block (state);
  label->pc = state->code_length;
  if (state->blocks == NULL)
    state->blocks = label;
  else
    state->last_block->next = label;
  state->last_block = label;
  label->next = NULL;
  label->u.relocations = NULL;
}

static struct jcf_block *
get_jcf_label_here (state)
     struct jcf_partial *state;
{
  if (state->last_block != NULL && BUFFER_LENGTH (&state->bytecode) == 0)
    return state->last_block;
  else
    {
      struct jcf_block *label = gen_jcf_label (state);
      define_jcf_label (label, state);
      return label;
    }
}

/* Note a line number entry for the current PC and given LINE. */

static void
put_linenumber (line, state)
     int line;
     struct jcf_partial *state;
{
  struct jcf_block *label = get_jcf_label_here (state);
  if (label->linenumber > 0)
    {
      label = gen_jcf_label (state);
      define_jcf_label (label, state);
    }
  label->linenumber = line;
  state->linenumber_count++;
}

/* Allocate a new jcf_handler, for a catch clause that catches exceptions
   in the range (START_LABEL, END_LABEL). */

static struct jcf_handler *
alloc_handler (start_label, end_label, state)
     struct jcf_block *start_label;
     struct jcf_block *end_label;
     struct jcf_partial *state;
{
  struct jcf_handler *handler = (struct jcf_handler *)
    obstack_alloc (state->chunk_obstack, sizeof (struct jcf_handler));
  handler->start_label = start_label;
  handler->end_label = end_label;
  handler->handler_label = get_jcf_label_here (state);
  if (state->handlers == NULL)
    state->handlers = handler;
  else
    state->last_handler->next = handler;
  state->last_handler = handler;
  handler->next = NULL;
  state->num_handlers++;
  return handler;
}


/* The index of jvm local variable allocated for this DECL.
   This is assigned when generating .class files;
   contrast DECL_LOCAL_SLOT_NUMBER which is set when *reading* a .class file.
   (We don't allocate DECL_LANG_SPECIFIC for locals from Java sourc code.) */

#define DECL_LOCAL_INDEX(DECL) DECL_ALIGN(DECL)

struct localvar_info
{
  struct localvar_info *next;

  tree decl;
  struct jcf_block *start_label;
  struct jcf_block *end_label;
};

#define localvar_buffer ((struct localvar_info**) state->localvars.data)
#define localvar_max \
  ((struct localvar_info**) state->localvars.ptr - localvar_buffer)

static void
localvar_alloc (decl, state)
     tree decl;
     struct jcf_partial *state;
{
  struct jcf_block *start_label = get_jcf_label_here (state);
  int wide = TYPE_IS_WIDE (TREE_TYPE (decl));
  int index;
  register struct localvar_info *info;
  register struct localvar_info **ptr = localvar_buffer;
  register struct localvar_info **limit
    = (struct localvar_info**) state->localvars.ptr;
  for (index = 0;  ptr < limit;  index++, ptr++)
    {
      if (ptr[0] == NULL
	  && (! wide || ((ptr+1) < limit && ptr[1] == NULL)))
	break;
    }
  if (ptr == limit)
    {
      buffer_grow (&state->localvars, 2 * sizeof (struct localvar_info*));
      ptr = (struct localvar_info**) state->localvars.data + index;
      state->localvars.ptr = (unsigned char *) (ptr + 1 + wide);
    }
  info = (struct localvar_info *)
    obstack_alloc (state->chunk_obstack, sizeof (struct localvar_info));
  ptr[0] = info;
  if (wide)
    ptr[1] = (struct localvar_info *)(~0);
  DECL_LOCAL_INDEX (decl) = index;
  info->decl = decl;
  info->start_label = start_label;

  if (debug_info_level > DINFO_LEVEL_TERSE
      && DECL_NAME (decl) != NULL_TREE)
    {
      /* Generate debugging info. */
      info->next = NULL;
      if (state->last_lvar != NULL)
	state->last_lvar->next = info;
      else
	state->first_lvar = info;
      state->last_lvar = info;
      state->lvar_count++;
    }
}

static void
localvar_free (decl, state)
     tree decl;     
     struct jcf_partial *state;
{
  struct jcf_block *end_label = get_jcf_label_here (state);
  int index = DECL_LOCAL_INDEX (decl);
  register struct localvar_info **ptr = &localvar_buffer [index];
  register struct localvar_info *info = *ptr;
  int wide = TYPE_IS_WIDE (TREE_TYPE (decl));

  info->end_label = end_label;

  if (info->decl != decl)
    abort ();
  ptr[0] = NULL;
  if (wide)
    {
      if (ptr[1] !=  (struct localvar_info *)(~0))
	abort ();
      ptr[1] = NULL;
    }
}


#define STACK_TARGET 1
#define IGNORE_TARGET 2

/* Get the access flags of a class (TYPE_DECL), a method (FUNCTION_DECL), or
   a field (FIELD_DECL or VAR_DECL, if static), as encoded in a .class file. */

static int
get_access_flags (decl)
    tree decl;
{
  int flags = 0;
  int isfield = TREE_CODE (decl) == FIELD_DECL || TREE_CODE (decl) == VAR_DECL;
  if (CLASS_PUBLIC (decl))  /* same as FIELD_PUBLIC and METHOD_PUBLIC */
    flags |= ACC_PUBLIC;
  if (CLASS_FINAL (decl))  /* same as FIELD_FINAL and METHOD_FINAL */
    flags |= ACC_FINAL;
  if (isfield || TREE_CODE (decl) == FUNCTION_DECL)
    {
      if (TREE_PROTECTED (decl))
	flags |= ACC_PROTECTED;
      if (TREE_PRIVATE (decl))
	flags |= ACC_PRIVATE;
    }
  else if (TREE_CODE (decl) == TYPE_DECL)
    {
      if (CLASS_SUPER (decl))
	flags |= ACC_SUPER;
      if (CLASS_ABSTRACT (decl))
	flags |= ACC_ABSTRACT;
      if (CLASS_INTERFACE (decl))
	flags |= ACC_INTERFACE;
    }
  else
    fatal ("internal error - bad argument to get_access_flags");
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      if (METHOD_NATIVE (decl))
	flags |= ACC_NATIVE;
      if (METHOD_STATIC (decl))
	flags |= ACC_STATIC;
      if (METHOD_SYNCHRONIZED (decl))
	flags |= ACC_SYNCHRONIZED;
      if (METHOD_ABSTRACT (decl))
	flags |= ACC_ABSTRACT;
    }
  if (isfield)
    {
      if (FIELD_STATIC (decl))
	flags |= ACC_STATIC;
      if (FIELD_VOLATILE (decl))
	flags |= ACC_VOLATILE;
      if (FIELD_TRANSIENT (decl))
	flags |= ACC_TRANSIENT;
    }
  return flags;
}

/* Write the list of segments starting at CHUNKS to STREAM. */

static void
write_chunks (stream, chunks)
     FILE* stream;
     struct chunk *chunks;
{
  for (;  chunks != NULL;  chunks = chunks->next)
    fwrite (chunks->data, chunks->size, 1, stream);
}

/* Push a 1-word constant in the constant pool at the given INDEX.
   (Caller is responsible for doing NOTE_PUSH.) */

static void
push_constant1 (index, state)
     HOST_WIDE_INT index;
     struct jcf_partial *state;
{
  RESERVE (3);
  if (index < 256)
    {
      OP1 (OPCODE_ldc);
      OP1 (index);
    }
  else
    {
      OP1 (OPCODE_ldc_w);
      OP2 (index);
    }
}

/* Push a 2-word constant in the constant pool at the given INDEX.
   (Caller is responsible for doing NOTE_PUSH.) */

static void
push_constant2 (index, state)
     HOST_WIDE_INT index;
     struct jcf_partial *state;
{
  RESERVE (3);
  OP1 (OPCODE_ldc2_w);
  OP2 (index);
}

/* Push 32-bit integer constant on VM stack.
   Caller is responsible for doing NOTE_PUSH. */

static void
push_int_const (i, state)
     HOST_WIDE_INT i;
     struct jcf_partial *state;
{
  RESERVE(3);
  if (i >= -1 && i <= 5)
    OP1(OPCODE_iconst_0 + i);
  else if (i >= -128 && i < 128)
    {
      OP1(OPCODE_bipush);
      OP1(i);
    }
  else if (i >= -32768 && i < 32768)
    {
      OP1(OPCODE_sipush);
      OP2(i);
    }
  else
    {
      i = find_constant1 (&state->cpool, CONSTANT_Integer, 
			  (jword)(i & 0xFFFFFFFF));
      push_constant1 (i, state);
    }
}

static int
find_constant_wide (lo, hi, state)
     HOST_WIDE_INT lo, hi;
     struct jcf_partial *state;
{
  HOST_WIDE_INT w1, w2;
  lshift_double (lo, hi, -32, 64, &w1, &w2, 1);
  return find_constant2 (&state->cpool, CONSTANT_Long,
			 (jword)(w1 & 0xFFFFFFFF), (jword)(lo & 0xFFFFFFFF));
}

/* Find or allocate a constant pool entry for the given VALUE.
   Return the index in the constant pool. */

static int
find_constant_index (value, state)
     tree value;
     struct jcf_partial *state;
{
  if (TREE_CODE (value) == INTEGER_CST)
    {
      if (TYPE_PRECISION (TREE_TYPE (value)) <= 32)
	return find_constant1 (&state->cpool, CONSTANT_Integer,
			       (jword)(TREE_INT_CST_LOW (value) & 0xFFFFFFFF));
      else
	return find_constant_wide (TREE_INT_CST_LOW (value),
				   TREE_INT_CST_HIGH (value), state);
    }
  else if (TREE_CODE (value) == REAL_CST)
    {
      long words[2];
      if (TYPE_PRECISION (TREE_TYPE (value)) == 32)
	{
	  words[0] = etarsingle (TREE_REAL_CST (value)) & 0xFFFFFFFF;
	  return find_constant1 (&state->cpool, CONSTANT_Float, 
				 (jword)words[0]);
	}
      else
	{
	  etardouble (TREE_REAL_CST (value), words);
	  return find_constant2 (&state->cpool, CONSTANT_Double,
				 (jword)(words[1-FLOAT_WORDS_BIG_ENDIAN] & 
					 0xFFFFFFFF),
				 (jword)(words[FLOAT_WORDS_BIG_ENDIAN] & 
					 0xFFFFFFFF));
	}
    }
  else if (TREE_CODE (value) == STRING_CST)
    {
      return find_string_constant (&state->cpool, value);
    }
  else
    fatal ("find_constant_index - bad type");
}

/* Push 64-bit long constant on VM stack.
   Caller is responsible for doing NOTE_PUSH. */

static void
push_long_const (lo, hi, state)
     HOST_WIDE_INT lo, hi;
     struct jcf_partial *state;
{
  if (hi == 0 && lo >= 0 && lo <= 1)
    {
      RESERVE(1);
      OP1(OPCODE_lconst_0 + lo);
    }
  else if ((hi == 0 && lo < 32768) || (hi == -1 && lo >= -32768))
      {
        push_int_const (lo, state);
        RESERVE (1);
        OP1 (OPCODE_i2l);
      }
  else
    push_constant2 (find_constant_wide (lo, hi, state), state);
}

static void
field_op (field, opcode, state)
     tree field;
     int opcode;
     struct jcf_partial *state;
{
  int index = find_fieldref_index (&state->cpool, field);
  RESERVE (3);
  OP1 (opcode);
  OP2 (index);
}

/* Returns an integer in the range 0 (for 'int') through 4 (for object
   reference) to 7 (for 'short') which matches the pattern of how JVM
   opcodes typically depend on the operand type. */

static int
adjust_typed_op (type, max)
     tree type;
     int max;
{
  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
    case RECORD_TYPE:   return 4;
    case BOOLEAN_TYPE:
      return TYPE_PRECISION (type) == 32 || max < 5 ? 0 : 5;
    case CHAR_TYPE:
      return TYPE_PRECISION (type) == 32 || max < 6 ? 0 : 6;
    case INTEGER_TYPE:
      switch (TYPE_PRECISION (type))
	{
	case  8:       return max < 5 ? 0 : 5;
	case 16:       return max < 7 ? 0 : 7;
	case 32:       return 0;
	case 64:       return 1;
	}
      break;
    case REAL_TYPE:
      switch (TYPE_PRECISION (type))
	{
	case 32:       return 2;
	case 64:       return 3;
	}
      break;
    default:
      break;
    }
  abort ();
}

static void
maybe_wide (opcode, index, state)
     int opcode, index;
     struct jcf_partial *state;
{
  if (index >= 256)
    {
      RESERVE (4);
      OP1 (OPCODE_wide);
      OP1 (opcode);
      OP2 (index);
    }
  else
    {
      RESERVE (2);
      OP1 (opcode);
      OP1 (index);
    }
}

/* Compile code to duplicate with offset, where
   SIZE is the size of the stack item to duplicate (1 or 2), abd
   OFFSET is where to insert the result (must be 0, 1, or 2).
   (The new words get inserted at stack[SP-size-offset].) */

static void
emit_dup (size, offset, state)
     int size, offset;
     struct jcf_partial *state;
{
  int kind;
  if (size == 0)
    return;
  RESERVE(1);
  if (offset == 0)
    kind = size == 1 ? OPCODE_dup : OPCODE_dup2;
  else if (offset == 1)
    kind = size == 1 ? OPCODE_dup_x1 : OPCODE_dup2_x1;
  else if (offset == 2)
    kind = size == 1 ? OPCODE_dup_x2 : OPCODE_dup2_x2;
  else
    abort();
  OP1 (kind);
  NOTE_PUSH (size);
}

static void
emit_pop (size, state)
     int size;
     struct jcf_partial *state;
{
  RESERVE (1);
  OP1 (OPCODE_pop - 1 + size);
}

static void
emit_iinc (var, value, state)
     tree var;
     HOST_WIDE_INT value;
     struct jcf_partial *state;
{
  int slot = DECL_LOCAL_INDEX (var);

  if (value < -128 || value > 127 || slot >= 256)
    {
      RESERVE (6);
      OP1 (OPCODE_wide);
      OP1 (OPCODE_iinc);
      OP2 (slot);
      OP2 (value);
    }
  else
    {
      RESERVE (3);
      OP1 (OPCODE_iinc);
      OP1 (slot);
      OP1 (value);
    }
}

static void
emit_load_or_store (var, opcode, state)
     tree var;    /* Variable to load from or store into. */
     int opcode;  /* Either OPCODE_iload or OPCODE_istore. */
     struct jcf_partial *state;
{
  tree type = TREE_TYPE (var);
  int kind = adjust_typed_op (type, 4);
  int index = DECL_LOCAL_INDEX (var);
  if (index <= 3)
    {
      RESERVE (1);
      OP1 (opcode + 5 + 4 * kind + index);    /* [ilfda]{load,store}_[0123] */
    }
  else
    maybe_wide (opcode + kind, index, state);  /* [ilfda]{load,store} */
}

static void
emit_load (var, state)
     tree var;
     struct jcf_partial *state;
{
  emit_load_or_store (var, OPCODE_iload, state);
  NOTE_PUSH (TYPE_IS_WIDE (TREE_TYPE (var)) ? 2 : 1);
}

static void
emit_store (var, state)
     tree var;
     struct jcf_partial *state;
{
  emit_load_or_store (var, OPCODE_istore, state);
  NOTE_POP (TYPE_IS_WIDE (TREE_TYPE (var)) ? 2 : 1);
}

static void
emit_unop (opcode, type, state)
     enum java_opcode opcode;
     tree type ATTRIBUTE_UNUSED;
     struct jcf_partial *state;
{
  RESERVE(1);
  OP1 (opcode);
}

static void
emit_binop (opcode, type, state)
     enum java_opcode opcode;
     tree type;
     struct jcf_partial *state;
{
  int size = TYPE_IS_WIDE (type) ? 2 : 1;
  RESERVE(1);
  OP1 (opcode);
  NOTE_POP (size);
}

static void
emit_reloc (value, kind, target, state)
     HOST_WIDE_INT value;
     int kind;
     struct jcf_block *target;
     struct jcf_partial *state;
{
  struct jcf_relocation *reloc = (struct jcf_relocation *)
    obstack_alloc (state->chunk_obstack, sizeof (struct jcf_relocation));
  struct jcf_block *block = state->last_block;
  reloc->next = block->u.relocations;
  block->u.relocations = reloc;
  reloc->offset = BUFFER_LENGTH (&state->bytecode);
  reloc->label = target;
  reloc->kind = kind;
  if (kind == 0 || kind == BLOCK_START_RELOC)
    OP4 (value);
  else if (kind != SWITCH_ALIGN_RELOC)
    OP2 (value);
}

static void
emit_switch_reloc (label, state)
     struct jcf_block *label;
     struct jcf_partial *state;
{
  emit_reloc (RELOCATION_VALUE_0, BLOCK_START_RELOC, label, state);
}

/* Similar to emit_switch_reloc,
   but re-uses an existing case reloc. */

static void
emit_case_reloc (reloc, state)
     struct jcf_relocation *reloc;
     struct jcf_partial *state;
{
  struct jcf_block *block = state->last_block;
  reloc->next = block->u.relocations;
  block->u.relocations = reloc;
  reloc->offset = BUFFER_LENGTH (&state->bytecode);
  reloc->kind = BLOCK_START_RELOC;
  OP4 (0);
}

/* Emit a conditional jump to TARGET with a 2-byte relative jump offset
   The opcode is OPCODE, the inverted opcode is INV_OPCODE. */

static void
emit_if (target, opcode, inv_opcode, state)
     struct jcf_block *target;
     int opcode, inv_opcode;
     struct jcf_partial *state;
{
  OP1 (opcode);
  /* value is 1 byte from reloc back to start of instruction.  */
  emit_reloc (RELOCATION_VALUE_1, - inv_opcode, target, state);
}

static void
emit_goto (target, state)
     struct jcf_block *target;
     struct jcf_partial *state;
{
  OP1 (OPCODE_goto);
  /* Value is 1 byte from reloc back to start of instruction.  */
  emit_reloc (RELOCATION_VALUE_1, OPCODE_goto_w, target, state);
}

static void
emit_jsr (target, state)
     struct jcf_block *target;
     struct jcf_partial *state;
{
  OP1 (OPCODE_jsr);
  /* Value is 1 byte from reloc back to start of instruction.  */
  emit_reloc (RELOCATION_VALUE_1, OPCODE_jsr_w, target, state);
}

/* Generate code to evaluate EXP.  If the result is true,
   branch to TRUE_LABEL; otherwise, branch to FALSE_LABEL.
   TRUE_BRANCH_FIRST is a code geneation hint that the
   TRUE_LABEL may follow right after this. (The idea is that we
   may be able to optimize away GOTO TRUE_LABEL; TRUE_LABEL:) */

static void
generate_bytecode_conditional (exp, true_label, false_label,
			       true_branch_first, state)
     tree exp;
     struct jcf_block *true_label;
     struct jcf_block *false_label;
     int true_branch_first;
     struct jcf_partial *state;
{
  tree exp0, exp1, type;
  int save_SP = state->code_SP;
  enum java_opcode op, negop;
  switch (TREE_CODE (exp))
    {
    case INTEGER_CST:
      emit_goto (integer_zerop (exp) ? false_label : true_label, state);
      break;
    case COND_EXPR:
      {
	struct jcf_block *then_label = gen_jcf_label (state);
	struct jcf_block *else_label = gen_jcf_label (state);
	int save_SP_before, save_SP_after;
	generate_bytecode_conditional (TREE_OPERAND (exp, 0),
				       then_label, else_label, 1, state);
	define_jcf_label (then_label, state);
	save_SP_before = state->code_SP;
	generate_bytecode_conditional (TREE_OPERAND (exp, 1),
				       true_label, false_label, 1, state);
	save_SP_after = state->code_SP;
	state->code_SP = save_SP_before;
	define_jcf_label (else_label, state);
	generate_bytecode_conditional (TREE_OPERAND (exp, 2),
				       true_label, false_label,
				       true_branch_first, state);
	if (state->code_SP != save_SP_after)
	  fatal ("internal error  non-matching SP");
      }
      break;
    case TRUTH_NOT_EXPR:
      generate_bytecode_conditional (TREE_OPERAND (exp, 0), 
				     false_label, true_label,
				     ! true_branch_first, state);
      break;
    case TRUTH_ANDIF_EXPR:
      {
	struct jcf_block *next_label = gen_jcf_label (state);
	generate_bytecode_conditional (TREE_OPERAND (exp, 0),
				       next_label, false_label, 1, state);
	define_jcf_label (next_label, state);
	generate_bytecode_conditional (TREE_OPERAND (exp, 1),
				       true_label, false_label, 1, state);
      }
      break;
    case TRUTH_ORIF_EXPR:
      {
	struct jcf_block *next_label = gen_jcf_label (state);
	generate_bytecode_conditional (TREE_OPERAND (exp, 0),
				       true_label, next_label, 1, state);
	define_jcf_label (next_label, state);
	generate_bytecode_conditional (TREE_OPERAND (exp, 1),
				       true_label, false_label, 1, state);
      }
      break;
    compare_1:
      /* Assuming op is one of the 2-operand if_icmp<COND> instructions,
	 set it to the corresponding 1-operand if<COND> instructions. */
      op = op - 6;
      /* FALLTHROUGH */
    compare_2:
      /* The opcodes with their inverses are allocated in pairs.
	 E.g.  The inverse of if_icmplt (161) is if_icmpge (162). */
      negop = (op & 1) ? op + 1 : op - 1;
    compare_2_ptr:
      if (true_branch_first)
	{
	  emit_if (false_label, negop, op, state);
	  emit_goto (true_label, state);
	}
      else
	{
	  emit_if (true_label, op, negop, state);
	  emit_goto (false_label, state);
	}
      break;
    case EQ_EXPR:
      op = OPCODE_if_icmpeq;
      goto compare;
    case NE_EXPR:
      op = OPCODE_if_icmpne;
      goto compare;
    case GT_EXPR:
      op = OPCODE_if_icmpgt;
      goto compare;
    case LT_EXPR:
      op = OPCODE_if_icmplt;
      goto compare;
    case GE_EXPR:
      op = OPCODE_if_icmpge;
      goto compare;
    case LE_EXPR:
      op = OPCODE_if_icmple;
      goto compare;
    compare:
      exp0 = TREE_OPERAND (exp, 0);
      exp1 = TREE_OPERAND (exp, 1);
      type = TREE_TYPE (exp0);
      switch (TREE_CODE (type))
	{
	  int opf;
	case POINTER_TYPE:  case RECORD_TYPE:
	  switch (TREE_CODE (exp))
	    {
	    case EQ_EXPR:  op = OPCODE_if_acmpeq;  break;
	    case NE_EXPR:  op = OPCODE_if_acmpne;  break;
	    default:  abort();
	    }
	  if (integer_zerop (exp1) || integer_zerop (exp0))
	    {
	      generate_bytecode_insns (integer_zerop (exp1) ? exp0 : exp1,
				       STACK_TARGET, state);
	      op = op + (OPCODE_ifnull - OPCODE_if_acmpeq);
	      negop = (op & 1) ? op - 1 : op + 1;
	      NOTE_POP (1);
	      goto compare_2_ptr;
	    }
	  generate_bytecode_insns (exp0, STACK_TARGET, state);
	  generate_bytecode_insns (exp1, STACK_TARGET, state);
	  NOTE_POP (2);
	  goto compare_2;
	case REAL_TYPE:
	  generate_bytecode_insns (exp0, STACK_TARGET, state);
	  generate_bytecode_insns (exp1, STACK_TARGET, state);
	  if (op == OPCODE_if_icmplt || op == OPCODE_if_icmple)
	    opf = OPCODE_fcmpg;
	  else
	    opf = OPCODE_fcmpl;
	  if (TYPE_PRECISION (type) > 32)
	    {
	      opf += 2;
	      NOTE_POP (4);
	    }
	  else
	    NOTE_POP (2);
	  RESERVE (1);
	  OP1 (opf);
	  goto compare_1;
	case INTEGER_TYPE:
	  if (TYPE_PRECISION (type) > 32)
	    {
	      generate_bytecode_insns (exp0, STACK_TARGET, state);
	      generate_bytecode_insns (exp1, STACK_TARGET, state);
	      NOTE_POP (4);
	      RESERVE (1);
	      OP1 (OPCODE_lcmp);
	      goto compare_1;
	    }
	  /* FALLTHOUGH */
	default:
	  if (integer_zerop (exp1))
	    {
	      generate_bytecode_insns (exp0, STACK_TARGET, state);
	      NOTE_POP (1);
	      goto compare_1;
	    }
	  if (integer_zerop (exp0))
	    {
	      switch (op)
		{
		case OPCODE_if_icmplt:
		case OPCODE_if_icmpge:
		  op += 2;
		  break;
		case OPCODE_if_icmpgt:
		case OPCODE_if_icmple:
		  op -= 2;
		  break;
		default:
		  break;
		}
	      generate_bytecode_insns (exp1, STACK_TARGET, state);
	      NOTE_POP (1);
	      goto compare_1;
	    }
	  generate_bytecode_insns (exp0, STACK_TARGET, state);
	  generate_bytecode_insns (exp1, STACK_TARGET, state);
	  NOTE_POP (2);
	  goto compare_2;
	}

    default:
      generate_bytecode_insns (exp, STACK_TARGET, state);
      NOTE_POP (1);
      if (true_branch_first)
	{
	  emit_if (false_label, OPCODE_ifeq, OPCODE_ifne, state);
	  emit_goto (true_label, state);
	}
      else
	{
	  emit_if (true_label, OPCODE_ifne, OPCODE_ifeq, state);
	  emit_goto (false_label, state);
	}
      break;
    }
  if (save_SP != state->code_SP)
    fatal ("internal error - SP mismatch");
}

/* Call pending cleanups i.e. those for surrounding CLEANUP_POINT_EXPRs
   but only as far out as LIMIT (since we are about to jump to the
   emit label that is LIMIT). */

static void
call_cleanups (limit, state)
     struct jcf_block *limit;
     struct jcf_partial *state;
{
  struct jcf_block *block = state->labeled_blocks;
  for (;  block != limit;  block = block->next)
    {
      if (block->pc == PENDING_CLEANUP_PC)
	emit_jsr (block, state);
    }
}

static void
generate_bytecode_return (exp, state)
     tree exp;
     struct jcf_partial *state;
{
  tree return_type = TREE_TYPE (TREE_TYPE (state->current_method));
  int returns_void = TREE_CODE (return_type) == VOID_TYPE;
  int op;
 again:
  if (exp != NULL)
    {
      switch (TREE_CODE (exp))
	{
 	case COMPOUND_EXPR:	
	  generate_bytecode_insns (TREE_OPERAND (exp, 0), IGNORE_TARGET,
				   state);
	  exp = TREE_OPERAND (exp, 1);
	  goto again;
	case COND_EXPR:
	  {
	    struct jcf_block *then_label = gen_jcf_label (state);
	    struct jcf_block *else_label = gen_jcf_label (state);
	    generate_bytecode_conditional (TREE_OPERAND (exp, 0),
					   then_label, else_label, 1, state);
	    define_jcf_label (then_label, state);
	    generate_bytecode_return (TREE_OPERAND (exp, 1), state);
	    define_jcf_label (else_label, state);
	    generate_bytecode_return (TREE_OPERAND (exp, 2), state);
	  }
	  return;
	default:
	  generate_bytecode_insns (exp,
				   returns_void ? IGNORE_TARGET
				   : STACK_TARGET, state);
	}
    }
  if (returns_void)
    {
      op = OPCODE_return;
      call_cleanups (NULL_PTR, state);
    }
  else
    {
      op = OPCODE_ireturn + adjust_typed_op (return_type, 4);
      if (state->num_finalizers > 0)
	{
	  if (state->return_value_decl == NULL_TREE)
	    {
	      state->return_value_decl
		= build_decl (VAR_DECL, NULL_TREE, TREE_TYPE (exp));
	      localvar_alloc (state->return_value_decl, state);
	    }
	  emit_store (state->return_value_decl, state);
	  call_cleanups (NULL_PTR, state);
	  emit_load (state->return_value_decl, state);
	  /* If we call localvar_free (state->return_value_decl, state),
	     then we risk the save decl erroneously re-used in the
	     finalizer.  Instead, we keep the state->return_value_decl
	     allocated through the rest of the method.  This is not
	     the greatest solution, but it is at least simple and safe. */
	}
    }
  RESERVE (1);
  OP1 (op);
}

/* Generate bytecode for sub-expression EXP of METHOD.
   TARGET is one of STACK_TARGET or IGNORE_TARGET. */

static void
generate_bytecode_insns (exp, target, state)
     tree exp;
     int target;
     struct jcf_partial *state;
{
  tree type;
  enum java_opcode jopcode;
  int op;
  HOST_WIDE_INT value;
  int post_op;
  int size;
  int offset;

  if (exp == NULL && target == IGNORE_TARGET)
    return;

  type = TREE_TYPE (exp);

  switch (TREE_CODE (exp))
    {
    case BLOCK:
      if (BLOCK_EXPR_BODY (exp))
	{
	  tree local;
	  tree body = BLOCK_EXPR_BODY (exp);
	  for (local = BLOCK_EXPR_DECLS (exp); local; )
	    {
	      tree next = TREE_CHAIN (local);
	      localvar_alloc (local, state);
	      local = next;
	    }
	  /* Avoid deep recursion for long blocks. */
	  while (TREE_CODE (body) == COMPOUND_EXPR)
	    {
	      generate_bytecode_insns (TREE_OPERAND (body, 0), target, state);
	      body = TREE_OPERAND (body, 1);
	    }
	  generate_bytecode_insns (body, target, state);
	  for (local = BLOCK_EXPR_DECLS (exp); local; )
	    {
	      tree next = TREE_CHAIN (local);
	      localvar_free (local, state);
	      local = next;
	    }
	}
      break;
      case COMPOUND_EXPR:	
      generate_bytecode_insns (TREE_OPERAND (exp, 0), IGNORE_TARGET, state);
      generate_bytecode_insns (TREE_OPERAND (exp, 1), target, state);
      break;
    case EXPR_WITH_FILE_LOCATION:
      {
	char *saved_input_filename = input_filename;
	tree body = EXPR_WFL_NODE (exp);
	int saved_lineno = lineno;
	if (body == empty_stmt_node)
	  break;
	input_filename = EXPR_WFL_FILENAME (exp);
	lineno = EXPR_WFL_LINENO (exp);
	if (EXPR_WFL_EMIT_LINE_NOTE (exp) && lineno > 0
	    && debug_info_level > DINFO_LEVEL_NONE)
	  put_linenumber (lineno, state);
	generate_bytecode_insns (body, target, state);
	input_filename = saved_input_filename;
	lineno = saved_lineno;
      }
      break;
    case INTEGER_CST:
      if (target == IGNORE_TARGET) ; /* do nothing */
      else if (TREE_CODE (type) == POINTER_TYPE)
	{
	  if (! integer_zerop (exp))
	    abort();
	  RESERVE(1);
	  OP1 (OPCODE_aconst_null);
	  NOTE_PUSH (1);
	}
      else if (TYPE_PRECISION (type) <= 32)
	{
	  push_int_const (TREE_INT_CST_LOW (exp), state);
	  NOTE_PUSH (1);
	}
      else
	{
	  push_long_const (TREE_INT_CST_LOW (exp), TREE_INT_CST_HIGH (exp),
			   state);
	  NOTE_PUSH (2);
	}
      break;
    case REAL_CST:
      {
	int prec = TYPE_PRECISION (type) >> 5;
	RESERVE(1);
	if (real_zerop (exp))
	  OP1 (prec == 1 ? OPCODE_fconst_0 : OPCODE_dconst_0);
	else if (real_onep (exp))
	  OP1 (prec == 1 ? OPCODE_fconst_1 : OPCODE_dconst_1);
	/* FIXME Should also use fconst_2 for 2.0f.
	   Also, should use iconst_2/ldc followed by i2f/i2d
	   for other float/double when the value is a small integer. */
	else
	  {
	    offset = find_constant_index (exp, state);
	    if (prec == 1)
	      push_constant1 (offset, state);
	    else
	      push_constant2 (offset, state);
	  }
	NOTE_PUSH (prec);
      }
      break;
    case STRING_CST:
      push_constant1 (find_string_constant (&state->cpool, exp), state);
      NOTE_PUSH (1);
      break;
    case VAR_DECL:
      if (TREE_STATIC (exp))
	{
	  field_op (exp, OPCODE_getstatic, state);
	  NOTE_PUSH (TYPE_IS_WIDE (TREE_TYPE (exp)) ? 2 : 1);
	  break;
	}
      /* ... fall through ... */
    case PARM_DECL:
      emit_load (exp, state);
      break;
    case NON_LVALUE_EXPR:
    case INDIRECT_REF:
      generate_bytecode_insns (TREE_OPERAND (exp, 0), target, state);
      break;
    case ARRAY_REF:
      generate_bytecode_insns (TREE_OPERAND (exp, 0), target, state);
      generate_bytecode_insns (TREE_OPERAND (exp, 1), target, state);
      if (target != IGNORE_TARGET)
	{
	  jopcode = OPCODE_iaload + adjust_typed_op (type, 7);
	  RESERVE(1);
	  OP1 (jopcode);
	  if (! TYPE_IS_WIDE (type))
	    NOTE_POP (1);
	}
      break;
    case COMPONENT_REF:
      {
	tree obj = TREE_OPERAND (exp, 0);
	tree field = TREE_OPERAND (exp, 1);
	int is_static = FIELD_STATIC (field);
	generate_bytecode_insns (obj,
				 is_static ? IGNORE_TARGET : target, state);
	if (target != IGNORE_TARGET)
	  {
	    if (DECL_NAME (field) == length_identifier_node && !is_static
		&& TYPE_ARRAY_P (TREE_TYPE (obj)))
	      {
		RESERVE (1);
		OP1 (OPCODE_arraylength);
	      }
	    else
	      {
		field_op (field, is_static ? OPCODE_getstatic : OPCODE_getfield,
			  state);
		if (! is_static)
		  NOTE_POP (1);
		NOTE_PUSH (TYPE_IS_WIDE (TREE_TYPE (field)) ? 2 : 1);
	      }
	  }
      }
      break;
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case GT_EXPR:
    case LT_EXPR:
    case GE_EXPR:
    case LE_EXPR:
      {
	struct jcf_block *then_label = gen_jcf_label (state);
	struct jcf_block *else_label = gen_jcf_label (state);
	struct jcf_block *end_label = gen_jcf_label (state);
	generate_bytecode_conditional (exp,
				       then_label, else_label, 1, state);
	define_jcf_label (then_label, state);
	push_int_const (1, state);
	emit_goto (end_label, state);
	define_jcf_label (else_label, state);
	push_int_const (0, state);
	define_jcf_label (end_label, state);
	NOTE_PUSH (1);
      }
      break;
    case COND_EXPR:
      {
	struct jcf_block *then_label = gen_jcf_label (state);
	struct jcf_block *else_label = gen_jcf_label (state);
	struct jcf_block *end_label = gen_jcf_label (state);
	generate_bytecode_conditional (TREE_OPERAND (exp, 0),
				       then_label, else_label, 1, state);
	define_jcf_label (then_label, state);
	generate_bytecode_insns (TREE_OPERAND (exp, 1), target, state);
	if (CAN_COMPLETE_NORMALLY (TREE_OPERAND (exp, 1))
	    /* Not all expressions have CAN_COMPLETE_NORMALLY set properly. */
	    || TREE_CODE (TREE_TYPE (exp)) != VOID_TYPE)
	  emit_goto (end_label, state);
	define_jcf_label (else_label, state);
	generate_bytecode_insns (TREE_OPERAND (exp, 2), target, state);
	define_jcf_label (end_label, state);

	/* COND_EXPR can be used in a binop. The stack must be adjusted. */
	if (TREE_TYPE (exp) != void_type_node)
	  NOTE_POP (TYPE_PRECISION (TREE_TYPE (exp)) > 32 ? 2 : 1);
      }
      break;
    case CASE_EXPR:
      {
	struct jcf_switch_state *sw_state = state->sw_state;
	struct jcf_relocation *reloc = (struct jcf_relocation *)
	  obstack_alloc (state->chunk_obstack, sizeof (struct jcf_relocation));
	HOST_WIDE_INT case_value = TREE_INT_CST_LOW (TREE_OPERAND (exp, 0));
	reloc->kind = 0;
	reloc->label = get_jcf_label_here (state);
	reloc->offset = case_value;
	reloc->next = sw_state->cases;
	sw_state->cases = reloc;
	if (sw_state->num_cases == 0)
	  {
	    sw_state->min_case = case_value;
	    sw_state->max_case = case_value;
	  }
	else
	  {
	    if (case_value < sw_state->min_case)
	      sw_state->min_case = case_value;
	    if (case_value > sw_state->max_case)
	      sw_state->max_case = case_value;
	  }
	sw_state->num_cases++;
      }
      break;
    case DEFAULT_EXPR:
      state->sw_state->default_label = get_jcf_label_here (state);
      break;

    case SWITCH_EXPR:
      {
	/* The SWITCH_EXPR has three parts, generated in the following order:
	   1.  the switch_expression (the value used to select the correct case);
	   2.  the switch_body;
	   3.  the switch_instruction (the tableswitch/loopupswitch instruction.).
	   After code generation, we will re-order then in the order 1, 3, 2.
	   This is to avoid an extra GOTOs. */
	struct jcf_switch_state sw_state;
	struct jcf_block *expression_last; /* Last block of the switch_expression. */
	struct jcf_block *body_last; /* Last block of the switch_body. */
	struct jcf_block *switch_instruction;  /* First block of switch_instruction. */
	struct jcf_block *instruction_last; /* Last block of the switch_instruction. */
	struct jcf_block *body_block;
	int switch_length;
	sw_state.prev = state->sw_state;
	state->sw_state = &sw_state;
	sw_state.cases = NULL;
	sw_state.num_cases = 0;
	sw_state.default_label = NULL;
	generate_bytecode_insns (TREE_OPERAND (exp, 0), STACK_TARGET, state);
	expression_last = state->last_block;
	body_block = get_jcf_label_here (state);  /* Force a new block here. */
	generate_bytecode_insns (TREE_OPERAND (exp, 1), IGNORE_TARGET, state);
	body_last = state->last_block;

	switch_instruction = gen_jcf_label (state);
	define_jcf_label (switch_instruction, state);
	if (sw_state.default_label == NULL)
	  sw_state.default_label = gen_jcf_label (state);

	if (sw_state.num_cases <= 1)
	  {
	    if (sw_state.num_cases == 0)
	      {
		emit_pop (1, state);
		NOTE_POP (1);
	      }
	    else
	      {
		push_int_const (sw_state.cases->offset, state);
		emit_if (sw_state.cases->label,
			 OPCODE_ifeq, OPCODE_ifne, state);
	      }
	    emit_goto (sw_state.default_label, state);
	  }
	else
	  {
	    HOST_WIDE_INT i;
	    /* Copy the chain of relocs into a sorted array. */
	    struct jcf_relocation **relocs = (struct jcf_relocation **)
	      xmalloc (sw_state.num_cases * sizeof (struct jcf_relocation *));
	    /* The relocs arrays is a buffer with a gap.
	       The assumption is that cases will normally come in "runs". */
	    int gap_start = 0;
	    int gap_end = sw_state.num_cases;
	    struct jcf_relocation *reloc;
	    for (reloc = sw_state.cases;  reloc != NULL;  reloc = reloc->next)
	      {
		HOST_WIDE_INT case_value = reloc->offset;
		while (gap_end < sw_state.num_cases)
		  {
		    struct jcf_relocation *end = relocs[gap_end];
		    if (case_value <= end->offset)
		      break;
		    relocs[gap_start++] = end;
		    gap_end++;
		  }
		while (gap_start > 0)
		  {
		    struct jcf_relocation *before = relocs[gap_start-1];
		    if (case_value >= before->offset)
		      break;
		    relocs[--gap_end] = before;
		    gap_start--;
		  }
		relocs[gap_start++] = reloc;
		/* Note we don't check for duplicates.  FIXME! */
	      }

	    if (2 * sw_state.num_cases
		>= sw_state.max_case - sw_state.min_case)
	      { /* Use tableswitch. */
		int index = 0;
		RESERVE (13 + 4 * (sw_state.max_case - sw_state.min_case + 1));
		OP1 (OPCODE_tableswitch);
		emit_reloc (RELOCATION_VALUE_0, 
			    SWITCH_ALIGN_RELOC, NULL, state);
		emit_switch_reloc (sw_state.default_label, state);
		OP4 (sw_state.min_case);
		OP4 (sw_state.max_case);
		for (i = sw_state.min_case; ; )
		  {
		    reloc = relocs[index];
		    if (i == reloc->offset)
		      {
			emit_case_reloc (reloc, state);
			if (i == sw_state.max_case)
			  break;
			index++;
		      }
		    else
		      emit_switch_reloc (sw_state.default_label, state);
		    i++;
		  }
	      }
	    else
	      { /* Use lookupswitch. */
		RESERVE(9 + 8 * sw_state.num_cases);
		OP1 (OPCODE_lookupswitch);
		emit_reloc (RELOCATION_VALUE_0,
			    SWITCH_ALIGN_RELOC, NULL, state);
		emit_switch_reloc (sw_state.default_label, state);
		OP4 (sw_state.num_cases);
		for (i = 0;  i < sw_state.num_cases;  i++)
		  {
		    struct jcf_relocation *reloc = relocs[i];
		    OP4 (reloc->offset);
		    emit_case_reloc (reloc, state);
		  }
	      }
	    free (relocs);
	  }

	instruction_last = state->last_block;
	if (sw_state.default_label->pc < 0)
	  define_jcf_label (sw_state.default_label, state);
	else /* Force a new block. */
	  sw_state.default_label = get_jcf_label_here (state);
	/* Now re-arrange the blocks so the switch_instruction
	   comes before the switch_body. */
	switch_length = state->code_length - switch_instruction->pc;
	switch_instruction->pc = body_block->pc;
	instruction_last->next = body_block;
	instruction_last->v.chunk->next = body_block->v.chunk;
	expression_last->next = switch_instruction;
	expression_last->v.chunk->next = switch_instruction->v.chunk;
	body_last->next = sw_state.default_label;
	body_last->v.chunk->next = NULL;
	state->chunk = body_last->v.chunk;
	for (;  body_block != sw_state.default_label;  body_block = body_block->next)
	  body_block->pc += switch_length;

	state->sw_state = sw_state.prev;
	break;
      }

    case RETURN_EXPR:
      exp = TREE_OPERAND (exp, 0);
      if (exp == NULL_TREE)
	exp = empty_stmt_node;
      else if (TREE_CODE (exp) != MODIFY_EXPR) 
	abort ();
      else
	exp = TREE_OPERAND (exp, 1);
      generate_bytecode_return (exp, state);
      break;
    case LABELED_BLOCK_EXPR:
      {
	struct jcf_block *end_label = gen_jcf_label (state);
	end_label->next = state->labeled_blocks;
	state->labeled_blocks = end_label;
	end_label->pc = PENDING_EXIT_PC;
	end_label->u.labeled_block = exp;
	if (LABELED_BLOCK_BODY (exp))
	  generate_bytecode_insns (LABELED_BLOCK_BODY (exp), target, state);
	if (state->labeled_blocks != end_label)
	  abort();
	state->labeled_blocks = end_label->next;
	define_jcf_label (end_label, state);
      }
      break;
    case LOOP_EXPR:
      {
	tree body = TREE_OPERAND (exp, 0);
#if 0
	if (TREE_CODE (body) == COMPOUND_EXPR
	    && TREE_CODE (TREE_OPERAND (body, 0)) == EXIT_EXPR)
	  {
	    /* Optimize:  H: if (TEST) GOTO L; BODY; GOTO H; L:
	       to:  GOTO L;  BODY;  L:  if (!TEST) GOTO L; */
	    struct jcf_block *head_label;
	    struct jcf_block *body_label;
	    struct jcf_block *end_label = gen_jcf_label (state);
	    struct jcf_block *exit_label = state->labeled_blocks;
	    head_label = gen_jcf_label (state);
	    emit_goto (head_label, state);
	    body_label = get_jcf_label_here (state);
	    generate_bytecode_insns (TREE_OPERAND (body, 1), target, state);
	    define_jcf_label (head_label, state);
	    generate_bytecode_conditional (TREE_OPERAND (body, 0),
					   end_label, body_label, 1, state);
	    define_jcf_label (end_label, state);
	  }
	else
#endif
	  {
	    struct jcf_block *head_label = get_jcf_label_here (state);
	    generate_bytecode_insns (body, IGNORE_TARGET, state);
	    emit_goto (head_label, state);
	  }
      }
      break;
    case EXIT_EXPR:
      {
	struct jcf_block *label = state->labeled_blocks;
	struct jcf_block *end_label = gen_jcf_label (state);
	generate_bytecode_conditional (TREE_OPERAND (exp, 0),
				       label, end_label, 0, state);
	define_jcf_label (end_label, state);
      }
      break;
    case EXIT_BLOCK_EXPR:
      {
	struct jcf_block *label = state->labeled_blocks;
	if (TREE_OPERAND (exp, 1) != NULL) goto notimpl;
	while (label->u.labeled_block != TREE_OPERAND (exp, 0))
	  label = label->next;
	call_cleanups (label, state);
	emit_goto (label, state);
      }
      break;

    case PREDECREMENT_EXPR:  value = -1; post_op = 0;  goto increment;
    case PREINCREMENT_EXPR:  value =  1; post_op = 0;  goto increment;
    case POSTDECREMENT_EXPR: value = -1; post_op = 1;  goto increment;
    case POSTINCREMENT_EXPR: value =  1; post_op = 1;  goto increment;
    increment:

      exp = TREE_OPERAND (exp, 0);
      type = TREE_TYPE (exp);
      size = TYPE_IS_WIDE (type) ? 2 : 1;
      if ((TREE_CODE (exp) == VAR_DECL || TREE_CODE (exp) == PARM_DECL)
	  && ! TREE_STATIC (exp)
	  && TREE_CODE (type) == INTEGER_TYPE
	  && TYPE_PRECISION (type) == 32)
	{
	  if (target != IGNORE_TARGET && post_op)
	    emit_load (exp, state);
	  emit_iinc (exp, value, state);
	  if (target != IGNORE_TARGET && ! post_op)
	    emit_load (exp, state);
	  break;
	}
      if (TREE_CODE (exp) == COMPONENT_REF)
	{
	  generate_bytecode_insns (TREE_OPERAND (exp, 0), STACK_TARGET, state);
	  emit_dup (1, 0, state);
	  /* Stack:  ..., objectref, objectref. */
	  field_op (TREE_OPERAND (exp, 1), OPCODE_getfield, state);
	  NOTE_PUSH (size-1);
	  /* Stack:  ..., objectref, oldvalue. */
	  offset = 1;
	}
      else if (TREE_CODE (exp) == ARRAY_REF)
	{
	  generate_bytecode_insns (TREE_OPERAND (exp, 0), STACK_TARGET, state);
	  generate_bytecode_insns (TREE_OPERAND (exp, 1), STACK_TARGET, state);
	  emit_dup (2, 0, state);
	  /* Stack:  ..., array, index, array, index. */
	  jopcode = OPCODE_iaload + adjust_typed_op (TREE_TYPE (exp), 7);
	  RESERVE(1);
	  OP1 (jopcode);
	  NOTE_POP (2-size);
	  /* Stack:  ..., array, index, oldvalue. */
	  offset = 2;
	}
      else if (TREE_CODE (exp) == VAR_DECL || TREE_CODE (exp) == PARM_DECL)
	{
	  generate_bytecode_insns (exp, STACK_TARGET, state);
	  /* Stack:  ..., oldvalue. */
	  offset = 0;
	}
      else
	abort ();

      if (target != IGNORE_TARGET && post_op)
	emit_dup (size, offset, state);
      /* Stack, if ARRAY_REF:  ..., [result, ] array, index, oldvalue. */
      /* Stack, if COMPONENT_REF:  ..., [result, ] objectref, oldvalue. */
      /* Stack, otherwise:  ..., [result, ] oldvalue. */
      if (size == 1)
	push_int_const (value, state);
      else
	push_long_const (value, (HOST_WIDE_INT)(value >= 0 ? 0 : -1), state);
      NOTE_PUSH (size);
      emit_binop (OPCODE_iadd + adjust_typed_op (type, 3), type, state);
      if (target != IGNORE_TARGET && ! post_op)
	emit_dup (size, offset, state);
      /* Stack, if ARRAY_REF:  ..., [result, ] array, index, newvalue. */
      /* Stack, if COMPONENT_REF:  ..., [result, ] objectref, newvalue. */
      /* Stack, otherwise:  ..., [result, ] newvalue. */
      goto finish_assignment;

    case MODIFY_EXPR:
      {
	tree lhs = TREE_OPERAND (exp, 0);
	tree rhs = TREE_OPERAND (exp, 1);
	int offset = 0;

	/* See if we can use the iinc instruction. */
	if ((TREE_CODE (lhs) == VAR_DECL || TREE_CODE (lhs) == PARM_DECL)
	    && ! TREE_STATIC (lhs)
	    && TREE_CODE (TREE_TYPE (lhs)) == INTEGER_TYPE
	    && TYPE_PRECISION (TREE_TYPE (lhs)) == 32
	    && (TREE_CODE (rhs) == PLUS_EXPR || TREE_CODE (rhs) == MINUS_EXPR))
	  {
	    tree arg0 = TREE_OPERAND (rhs, 0);
	    tree arg1 = TREE_OPERAND (rhs, 1);
	    HOST_WIDE_INT min_value = -32768;
	    HOST_WIDE_INT max_value = 32767;
	    if (TREE_CODE (rhs) == MINUS_EXPR)
	      {
		min_value++;
		max_value++;
	      }
	    else if (arg1 == lhs)
	      {
		arg0 = arg1;
		arg1 = TREE_OPERAND (rhs, 0);
	      }
	    if (lhs == arg0 && TREE_CODE (arg1) == INTEGER_CST)
	      {
		HOST_WIDE_INT hi_value = TREE_INT_CST_HIGH (arg1);
		value = TREE_INT_CST_LOW (arg1);
		if ((hi_value == 0 && value <= max_value)
		    || (hi_value == -1 && value >= min_value))
		  {
		    if (TREE_CODE (rhs) == MINUS_EXPR)
		      value = -value;
		    emit_iinc (lhs, value, state);
		    break;
		  }
	      }
	  }

	if (TREE_CODE (lhs) == COMPONENT_REF)
	  {
	    generate_bytecode_insns (TREE_OPERAND (lhs, 0),
				     STACK_TARGET, state);
	    offset = 1;
	  }
	else if (TREE_CODE (lhs) == ARRAY_REF)
	  {
	    generate_bytecode_insns (TREE_OPERAND(lhs, 0),
				     STACK_TARGET, state);
	    generate_bytecode_insns (TREE_OPERAND(lhs, 1),
				     STACK_TARGET, state);
	    offset = 2;
	  }
	else
	  offset = 0;
	generate_bytecode_insns (rhs, STACK_TARGET, state);
	if (target != IGNORE_TARGET)
	  emit_dup (TYPE_IS_WIDE (type) ? 2 : 1 , offset, state);
	exp = lhs;
      }
      /* FALLTHOUGH */

    finish_assignment:
      if (TREE_CODE (exp) == COMPONENT_REF)
	{
	  tree field = TREE_OPERAND (exp, 1);
	  if (! FIELD_STATIC (field))
	    NOTE_POP (1);
	  field_op (field,
		    FIELD_STATIC (field) ? OPCODE_putstatic : OPCODE_putfield,
		    state);

	  NOTE_POP (TYPE_IS_WIDE (TREE_TYPE (field)) ? 2 : 1);
	}
      else if (TREE_CODE (exp) == VAR_DECL
	       || TREE_CODE (exp) == PARM_DECL)
	{
	  if (FIELD_STATIC (exp))
	    {
	      field_op (exp, OPCODE_putstatic, state);
	      NOTE_POP (TYPE_IS_WIDE (TREE_TYPE (exp)) ? 2 : 1);
	    }
	  else
	    emit_store (exp, state);
	}
      else if (TREE_CODE (exp) == ARRAY_REF)
	{
	  jopcode = OPCODE_iastore + adjust_typed_op (TREE_TYPE (exp), 7);
	  RESERVE(1);
	  OP1 (jopcode);
	  NOTE_POP (TYPE_IS_WIDE (TREE_TYPE (exp)) ? 4 : 3);
	}
      else
	fatal ("internal error (bad lhs to MODIFY_EXPR)");
      break;
    case PLUS_EXPR:
      jopcode = OPCODE_iadd;
      goto binop;
    case MINUS_EXPR:
      jopcode = OPCODE_isub;
      goto binop;
    case MULT_EXPR:
      jopcode = OPCODE_imul;
      goto binop;
    case TRUNC_DIV_EXPR:
    case RDIV_EXPR:
      jopcode = OPCODE_idiv;
      goto binop;
    case TRUNC_MOD_EXPR:
      jopcode = OPCODE_irem;
      goto binop;
    case LSHIFT_EXPR:   jopcode = OPCODE_ishl;   goto binop;
    case RSHIFT_EXPR:   jopcode = OPCODE_ishr;   goto binop;
    case URSHIFT_EXPR:  jopcode = OPCODE_iushr;  goto binop;
    case TRUTH_AND_EXPR:
    case BIT_AND_EXPR:  jopcode = OPCODE_iand;   goto binop;
    case TRUTH_OR_EXPR:
    case BIT_IOR_EXPR:  jopcode = OPCODE_ior;    goto binop;
    case TRUTH_XOR_EXPR:
    case BIT_XOR_EXPR:  jopcode = OPCODE_ixor;   goto binop;
    binop:
    {
      tree arg0 = TREE_OPERAND (exp, 0);
      tree arg1 = TREE_OPERAND (exp, 1);
      jopcode += adjust_typed_op (type, 3);
      if (arg0 == arg1 && TREE_CODE (arg0) == SAVE_EXPR)
	{
	  /* fold may (e.g) convert 2*x to x+x. */
	  generate_bytecode_insns (TREE_OPERAND (arg0, 0), target, state);
	  emit_dup (TYPE_PRECISION (TREE_TYPE (arg0)) > 32 ? 2 : 1, 0, state);
	}
      else
	{
	  generate_bytecode_insns (arg0, target, state);
	  generate_bytecode_insns (arg1, target, state);
	}
      /* For most binary operations, both operands and the result have the
	 same type.  Shift operations are different.  Using arg1's type
	 gets us the correct SP adjustment in all casesd. */
      if (target == STACK_TARGET)
	emit_binop (jopcode, TREE_TYPE (arg1), state);
      break;
    }
    case TRUTH_NOT_EXPR:
    case BIT_NOT_EXPR:
      generate_bytecode_insns (TREE_OPERAND (exp, 0), target, state);
      if (target == STACK_TARGET)
	{
	  int is_long = TYPE_PRECISION (TREE_TYPE (exp)) > 32;
	  push_int_const (TREE_CODE (exp) == BIT_NOT_EXPR ? -1 : 1, state); 
	  RESERVE (2);
	  if (is_long)
	    OP1 (OPCODE_i2l);
	  NOTE_PUSH (1 + is_long);
	  OP1 (OPCODE_ixor + is_long);
	  NOTE_POP (1 + is_long);
	}
      break;
    case NEGATE_EXPR:
      jopcode = OPCODE_ineg;
      jopcode += adjust_typed_op (type, 3);
      generate_bytecode_insns (TREE_OPERAND (exp, 0), target, state);
      if (target == STACK_TARGET)
	emit_unop (jopcode, type, state);
      break;
    case INSTANCEOF_EXPR:
      {
	int index = find_class_constant (&state->cpool, TREE_OPERAND (exp, 1));
	generate_bytecode_insns (TREE_OPERAND (exp, 0), target, state);
	RESERVE (3);
	OP1 (OPCODE_instanceof);
	OP2 (index);
      }
      break;
    case CONVERT_EXPR:
    case NOP_EXPR:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
      {
	tree src = TREE_OPERAND (exp, 0);
	tree src_type = TREE_TYPE (src);
	tree dst_type = TREE_TYPE (exp);
	generate_bytecode_insns (TREE_OPERAND (exp, 0), target, state);
	if (target == IGNORE_TARGET || src_type == dst_type)
	  break;
	if (TREE_CODE (dst_type) == POINTER_TYPE)
	  {
	    if (TREE_CODE (exp) == CONVERT_EXPR)
	      {
		int index = find_class_constant (&state->cpool, 
						 TREE_TYPE (dst_type));
		RESERVE (3);
		OP1 (OPCODE_checkcast);
		OP2 (index);
	      }
	  }
	else /* Convert numeric types. */
	  {
	    int wide_src = TYPE_PRECISION (src_type) > 32;
	    int wide_dst = TYPE_PRECISION (dst_type) > 32;
	    NOTE_POP (1 + wide_src);
	    RESERVE (1);
	    if (TREE_CODE (dst_type) == REAL_TYPE)
	      {
		if (TREE_CODE (src_type) == REAL_TYPE)
		  OP1 (wide_dst ? OPCODE_f2d : OPCODE_d2f);
		else if (TYPE_PRECISION (src_type) == 64)
		  OP1 (OPCODE_l2f + wide_dst);
		else
		  OP1 (OPCODE_i2f + wide_dst);
	      }
	    else /* Convert to integral type. */
	      {
		if (TREE_CODE (src_type) == REAL_TYPE)
		  OP1 (OPCODE_f2i + wide_dst + 3 * wide_src);
		else if (wide_dst)
		  OP1 (OPCODE_i2l);
		else if (wide_src)
		  OP1 (OPCODE_l2i);
		if (TYPE_PRECISION (dst_type) < 32)
		  {
		    RESERVE (1);
		    /* Already converted to int, if needed. */
		    if (TYPE_PRECISION (dst_type) <= 8)
		      OP1 (OPCODE_i2b);
		    else if (TREE_UNSIGNED (dst_type))
		      OP1 (OPCODE_i2c);
		    else
		      OP1 (OPCODE_i2s);
		  }
	      }
	    NOTE_PUSH (1 + wide_dst);
	  }
      }
      break;

    case CLEANUP_POINT_EXPR:
      {
	struct jcf_block *save_labeled_blocks = state->labeled_blocks;
	int can_complete = CAN_COMPLETE_NORMALLY (TREE_OPERAND (exp, 0));
	generate_bytecode_insns (TREE_OPERAND (exp, 0), IGNORE_TARGET, state);
	if (target != IGNORE_TARGET)
	  abort ();
	while (state->labeled_blocks != save_labeled_blocks)
	  {
	    struct jcf_block *finished_label = NULL;
	    tree return_link;
	    tree exception_type = build_pointer_type (throwable_type_node);
	    tree exception_decl = build_decl (VAR_DECL, NULL_TREE,
					      exception_type);
	    struct jcf_block *end_label = get_jcf_label_here (state);
	    struct jcf_block *label = state->labeled_blocks;
	    struct jcf_handler *handler;
	    tree cleanup = label->u.labeled_block;
	    state->labeled_blocks = label->next;
	    state->num_finalizers--;
	    if (can_complete)
	      {
		finished_label = gen_jcf_label (state);
		emit_jsr (label, state);
		emit_goto (finished_label, state);
		if (! CAN_COMPLETE_NORMALLY (cleanup))
		  can_complete = 0;
	      }
	    handler = alloc_handler (label->v.start_label, end_label, state);
	    handler->type = NULL_TREE;
	    localvar_alloc (exception_decl, state);
	    NOTE_PUSH (1);
            emit_store (exception_decl, state);
	    emit_jsr (label, state);
	    emit_load (exception_decl, state);
	    RESERVE (1);
	    OP1 (OPCODE_athrow);
	    NOTE_POP (1);

	    /* The finally block. */
	    return_link = build_decl (VAR_DECL, NULL_TREE,
				      return_address_type_node);
	    define_jcf_label (label, state);
	    NOTE_PUSH (1);
	    localvar_alloc (return_link, state);
	    emit_store (return_link, state);
	    generate_bytecode_insns (cleanup, IGNORE_TARGET, state);
	    maybe_wide (OPCODE_ret, DECL_LOCAL_INDEX (return_link), state);
	    localvar_free (return_link, state);
	    localvar_free (exception_decl, state);
	    if (finished_label != NULL)
	      define_jcf_label (finished_label, state);
	  }
      }
      break;

    case WITH_CLEANUP_EXPR:
      {
	struct jcf_block *label;
	generate_bytecode_insns (TREE_OPERAND (exp, 0), IGNORE_TARGET, state);
	label = gen_jcf_label (state);
	label->pc = PENDING_CLEANUP_PC;
	label->next = state->labeled_blocks;
	state->labeled_blocks = label;
	state->num_finalizers++;
	label->u.labeled_block = TREE_OPERAND (exp, 2);
	label->v.start_label = get_jcf_label_here (state);
	if (target != IGNORE_TARGET)
	  abort ();
      }
      break;

    case TRY_EXPR:
      {
	tree try_clause = TREE_OPERAND (exp, 0);
	struct jcf_block *start_label = get_jcf_label_here (state);
	struct jcf_block *end_label;  /* End of try clause. */
	struct jcf_block *finished_label = gen_jcf_label (state);
	tree clause = TREE_OPERAND (exp, 1);
	if (target != IGNORE_TARGET)
	  abort ();
	generate_bytecode_insns (try_clause, IGNORE_TARGET, state);
	end_label = get_jcf_label_here (state);
	if (CAN_COMPLETE_NORMALLY (try_clause))
	  emit_goto (finished_label, state);
	while (clause != NULL_TREE)
	  {
	    tree catch_clause = TREE_OPERAND (clause, 0);
	    tree exception_decl = BLOCK_EXPR_DECLS (catch_clause);
	    struct jcf_handler *handler = alloc_handler (start_label, end_label, state);
	    if (exception_decl == NULL_TREE)
	      handler->type = NULL_TREE;
	    else
	      handler->type = TREE_TYPE (TREE_TYPE (exception_decl));
	    generate_bytecode_insns (catch_clause, IGNORE_TARGET, state);
	    clause = TREE_CHAIN (clause);
	    if (CAN_COMPLETE_NORMALLY (catch_clause) && clause != NULL_TREE)
	      emit_goto (finished_label, state);
	  }
	define_jcf_label (finished_label, state);
      }
      break;
    case TRY_FINALLY_EXPR:
      {
	tree try_block = TREE_OPERAND (exp, 0);
	tree finally = TREE_OPERAND (exp, 1);
	struct jcf_block *finished_label = gen_jcf_label (state);
	struct jcf_block *finally_label = gen_jcf_label (state);
	struct jcf_block *start_label = get_jcf_label_here (state);
	tree return_link = build_decl (VAR_DECL, NULL_TREE,
				       return_address_type_node);
	tree exception_type = build_pointer_type (throwable_type_node);
	tree exception_decl = build_decl (VAR_DECL, NULL_TREE, exception_type);
	struct jcf_handler *handler;

	finally_label->pc = PENDING_CLEANUP_PC;
	finally_label->next = state->labeled_blocks;
	state->labeled_blocks = finally_label;
	state->num_finalizers++;

	generate_bytecode_insns (try_block, target, state);
	if (state->labeled_blocks != finally_label)
	  abort();
	state->labeled_blocks = finally_label->next;
	emit_jsr (finally_label, state);
	if (CAN_COMPLETE_NORMALLY (try_block))
	  emit_goto (finished_label, state);

	/* Handle exceptions. */
	localvar_alloc (return_link, state);
	handler = alloc_handler (start_label, NULL_PTR, state);
	handler->end_label = handler->handler_label;
	handler->type = NULL_TREE;
	localvar_alloc (exception_decl, state);
	NOTE_PUSH (1);
	emit_store (exception_decl, state);
	emit_jsr (finally_label, state);
	emit_load (exception_decl, state);
	RESERVE (1);
	OP1 (OPCODE_athrow);
	NOTE_POP (1);
	localvar_free (exception_decl, state);

	/* The finally block.  First save return PC into return_link. */
	define_jcf_label (finally_label, state);
	NOTE_PUSH (1);
	emit_store (return_link, state);

	generate_bytecode_insns (finally, IGNORE_TARGET, state);
	maybe_wide (OPCODE_ret, DECL_LOCAL_INDEX (return_link), state);
	localvar_free (return_link, state);
	define_jcf_label (finished_label, state);
      }
      break;
    case THROW_EXPR:
      generate_bytecode_insns (TREE_OPERAND (exp, 0), STACK_TARGET, state);
      RESERVE (1);
      OP1 (OPCODE_athrow);
      break;
    case NEW_ARRAY_INIT:
      {
	tree values = CONSTRUCTOR_ELTS (TREE_OPERAND (exp, 0));
	tree array_type = TREE_TYPE (TREE_TYPE (exp));
	tree element_type = TYPE_ARRAY_ELEMENT (array_type);
	HOST_WIDE_INT length = java_array_type_length (array_type);
	if (target == IGNORE_TARGET)
	  {
	    for ( ;  values != NULL_TREE;  values = TREE_CHAIN (values))
	      generate_bytecode_insns (TREE_VALUE (values), target, state);
	    break;
	  }
	push_int_const (length, state);
	NOTE_PUSH (1);
	RESERVE (3);
	if (JPRIMITIVE_TYPE_P (element_type))
	  {
	    int atype = encode_newarray_type (element_type);
	    OP1 (OPCODE_newarray);
	    OP1 (atype);
	  }
	else
	  {
	    int index = find_class_constant (&state->cpool,
					     TREE_TYPE (element_type));
	    OP1 (OPCODE_anewarray);
	    OP2 (index);
	  }
	offset = 0;
	jopcode = OPCODE_iastore + adjust_typed_op (element_type, 7);
	for ( ;  values != NULL_TREE;  values = TREE_CHAIN (values), offset++)
	  {
	    int save_SP = state->code_SP;
	    emit_dup (1, 0, state);
	    push_int_const (offset, state);
	    NOTE_PUSH (1);
	    generate_bytecode_insns (TREE_VALUE (values), STACK_TARGET, state);
	    RESERVE (1);
	    OP1 (jopcode);
	    state->code_SP = save_SP;
	  }
      }
      break;
    case NEW_CLASS_EXPR:
      {
	tree class = TREE_TYPE (TREE_TYPE (exp));
	int need_result = target != IGNORE_TARGET;
	int index = find_class_constant (&state->cpool, class);
	RESERVE (4);
	OP1 (OPCODE_new);
	OP2 (index);
	if (need_result)
	  OP1 (OPCODE_dup);
	NOTE_PUSH (1 + need_result);
      }
      /* ... fall though ... */
    case CALL_EXPR:
      {
	tree f = TREE_OPERAND (exp, 0);
	tree x = TREE_OPERAND (exp, 1);
	int save_SP = state->code_SP;
	int nargs;
	if (TREE_CODE (f) == ADDR_EXPR)
	  f = TREE_OPERAND (f, 0);
	if (f == soft_newarray_node)
	  {
	    int type_code = TREE_INT_CST_LOW (TREE_VALUE (x));
	    generate_bytecode_insns (TREE_VALUE (TREE_CHAIN (x)),
				     STACK_TARGET, state);
	    RESERVE (2);
	    OP1 (OPCODE_newarray);
	    OP1 (type_code);
	    break;
	  }
	else if (f == soft_multianewarray_node)
	  {
	    int ndims;
	    int idim;
	    int index = find_class_constant (&state->cpool,
					     TREE_TYPE (TREE_TYPE (exp)));
	    x = TREE_CHAIN (x);  /* Skip class argument. */
	    ndims = TREE_INT_CST_LOW (TREE_VALUE (x));
	    for (idim = ndims;  --idim >= 0; )
	      {
		x = TREE_CHAIN (x);
		generate_bytecode_insns (TREE_VALUE (x), STACK_TARGET, state);
	      }
	    RESERVE (4);
	    OP1 (OPCODE_multianewarray);
	    OP2 (index);
	    OP1 (ndims);
	    break;
	  }
	else if (f == soft_anewarray_node)
	  {
	    tree cl = TYPE_ARRAY_ELEMENT (TREE_TYPE (TREE_TYPE (exp)));
	    int index = find_class_constant (&state->cpool, TREE_TYPE (cl));
	    generate_bytecode_insns (TREE_VALUE (x), STACK_TARGET, state);
	    RESERVE (3);
	    OP1 (OPCODE_anewarray);
	    OP2 (index);
	    break;
	  }
	else if (f == soft_monitorenter_node
		 || f == soft_monitorexit_node
		 || f == throw_node[0]
		 || f == throw_node[1])
	  {
	    if (f == soft_monitorenter_node)
	      op = OPCODE_monitorenter;
	    else if (f == soft_monitorexit_node)
	      op = OPCODE_monitorexit;
	    else
	      op = OPCODE_athrow;
	    generate_bytecode_insns (TREE_VALUE (x), STACK_TARGET, state);
	    RESERVE (1);
	    OP1 (op);
	    NOTE_POP (1);
	    break;
	  }
	else if (exp == soft_exceptioninfo_call_node)
	  {
	    NOTE_PUSH (1);  /* Pushed by exception system. */
	    break;
	  }
	for ( ;  x != NULL_TREE;  x = TREE_CHAIN (x))
	  {
	    generate_bytecode_insns (TREE_VALUE (x), STACK_TARGET, state);
	  }
	nargs = state->code_SP - save_SP;
	state->code_SP = save_SP;
	if (f == soft_fmod_node)
	  {
	    RESERVE (1);
	    OP1 (OPCODE_drem);
	    NOTE_PUSH (2);
	    break;
	  }
	if (TREE_CODE (exp) == NEW_CLASS_EXPR)
	  NOTE_POP (1);  /* Pop implicit this. */
	if (TREE_CODE (f) == FUNCTION_DECL && DECL_CONTEXT (f) != NULL_TREE)
	  {
	    int index = find_methodref_index (&state->cpool, f);
	    int interface = 0;
	    RESERVE (5);
	    if (METHOD_STATIC (f))
	      OP1 (OPCODE_invokestatic);
	    else if (DECL_CONSTRUCTOR_P (f) || CALL_USING_SUPER (exp)
		|| METHOD_PRIVATE (f))
	      OP1 (OPCODE_invokespecial);
	    else if (CLASS_INTERFACE (TYPE_NAME (DECL_CONTEXT (f))))
	      {
		OP1 (OPCODE_invokeinterface);
		interface = 1;
	      }
	    else
	      OP1 (OPCODE_invokevirtual);
	    OP2 (index);
	    if (interface)
	      {
		OP1 (nargs);
		OP1 (0);
	      }
	    f = TREE_TYPE (TREE_TYPE (f));
	    if (TREE_CODE (f) != VOID_TYPE)
	      {
		int size = TYPE_IS_WIDE (f) ? 2 : 1;
		if (target == IGNORE_TARGET)
		  emit_pop (size, state);
		else
		  NOTE_PUSH (size);
	      }
	    break;
	  }
      }
      /* fall through */
    notimpl:
    default:
      error("internal error - tree code not implemented: %s",
	    tree_code_name [(int) TREE_CODE (exp)]);
    }
}

static void
perform_relocations (state)
     struct jcf_partial *state;
{
  struct jcf_block *block;
  struct jcf_relocation *reloc;
  int pc;
  int shrink;

  /* Before we start, the pc field of each block is an upper bound on
     the block's start pc (it may be less, if previous blocks need less
     than their maximum).

     The minimum size of each block is in the block's chunk->size. */

  /* First, figure out the actual locations of each block. */
  pc = 0;
  shrink = 0;
  for (block = state->blocks;  block != NULL;  block = block->next)
    {
      int block_size = block->v.chunk->size;

      block->pc = pc;

      /* Optimize GOTO L; L: by getting rid of the redundant goto.
	 Assumes relocations are in reverse order. */
      reloc = block->u.relocations;
      while (reloc != NULL
	     && reloc->kind == OPCODE_goto_w
	     && reloc->label->pc == block->next->pc
	     && reloc->offset + 2 == block_size)
	{
	  reloc = reloc->next;
	  block->u.relocations = reloc;
	  block->v.chunk->size -= 3;
	  block_size -= 3;
	  shrink += 3;
	}

      for (reloc = block->u.relocations;  reloc != NULL;  reloc = reloc->next)
	{
	  if (reloc->kind == SWITCH_ALIGN_RELOC)
	    {
	      /* We assume this is the first relocation in this block,
		 so we know its final pc. */
	      int where = pc + reloc->offset;
	      int pad = ((where + 3) & ~3) - where;
	      block_size += pad;
	    }
	  else if (reloc->kind < -1 || reloc->kind > BLOCK_START_RELOC)
	    {
	      int delta = reloc->label->pc - (pc + reloc->offset - 1);
	      int expand = reloc->kind > 0 ? 2 : 5;

	      if (delta > 0)
		delta -= shrink;
	      if (delta >= -32768 && delta <= 32767)
		{
		  shrink += expand;
		  reloc->kind = -1;
		}
	      else
		block_size += expand;
	    }
	}
      pc += block_size;
    }

  for (block = state->blocks;  block != NULL;  block = block->next)
    {
      struct chunk *chunk = block->v.chunk;
      int old_size = chunk->size;
      int next_pc = block->next == NULL ? pc : block->next->pc;
      int new_size = next_pc - block->pc;
      unsigned char *new_ptr;
      unsigned char *old_buffer = chunk->data;
      unsigned char *old_ptr = old_buffer + old_size;
      if (new_size != old_size)
	{
	  chunk->data = (unsigned char *)
	    obstack_alloc (state->chunk_obstack, new_size);
	  chunk->size = new_size;
	}
      new_ptr = chunk->data + new_size;

      /* We do the relocations from back to front, because
	 the relocations are in reverse order. */
      for (reloc = block->u.relocations; ; reloc = reloc->next)
	{
	  /* new_ptr and old_ptr point into the old and new buffers,
	     respectively.  (If no relocations cause the buffer to
	     grow, the buffer will be the same buffer, and new_ptr==old_ptr.)
	     The bytes at higher adress have been copied and relocations
	     handled; those at lower addresses remain to process. */

	  /* Lower old index of piece to be copied with no relocation.
	     I.e. high index of the first piece that does need relocation. */
	  int start = reloc == NULL ? 0
	    : reloc->kind == SWITCH_ALIGN_RELOC ? reloc->offset
	    : (reloc->kind == 0 || reloc->kind == BLOCK_START_RELOC)
	    ? reloc->offset + 4
	    : reloc->offset + 2;
	  int32 value;
	  int new_offset;
	  int n = (old_ptr - old_buffer) - start;
	  new_ptr -= n;
	  old_ptr -= n;
	  if (n > 0)
	    memcpy (new_ptr, old_ptr, n);
	  if (old_ptr == old_buffer)
	    break;

	  new_offset = new_ptr - chunk->data;
	  new_offset -= (reloc->kind == -1 ? 2 : 4);
	  if (reloc->kind == 0)
	    {
	      old_ptr -= 4;
	      value = GET_u4 (old_ptr);
	    }
	  else if (reloc->kind == BLOCK_START_RELOC)
	    {
	      old_ptr -= 4;
	      value = 0;
	      new_offset = 0;
	    }
	  else if (reloc->kind == SWITCH_ALIGN_RELOC)
	    {
	      int where = block->pc + reloc->offset;
	      int pad = ((where + 3) & ~3) - where;
	      while (--pad >= 0)
		*--new_ptr = 0;
	      continue;
	    }
	  else
	    {
	      old_ptr -= 2;
	      value = GET_u2 (old_ptr);
	    }
	  value += reloc->label->pc - (block->pc + new_offset);
	  *--new_ptr = (unsigned char) value;  value >>= 8;
	  *--new_ptr = (unsigned char) value;  value >>= 8;
	  if (reloc->kind != -1)
	    {
	      *--new_ptr = (unsigned char) value;  value >>= 8;
	      *--new_ptr = (unsigned char) value;
	    }
	  if (reloc->kind > BLOCK_START_RELOC)
	    {
	      /* Convert: OP TARGET to: OP_w TARGET;  (OP is goto or jsr). */
	      --old_ptr;
	      *--new_ptr = reloc->kind;
	    }
	  else if (reloc->kind < -1)
	    {
	      /* Convert: ifCOND TARGET to: ifNCOND T; goto_w TARGET; T: */
	      --old_ptr;
	      *--new_ptr = OPCODE_goto_w;
	      *--new_ptr = 3;
	      *--new_ptr = 0;
	      *--new_ptr = - reloc->kind;
	    }
	}
      if (new_ptr != chunk->data)
	fatal ("internal error - perform_relocations");
    }
  state->code_length = pc;
}

static void
init_jcf_state (state, work)
     struct jcf_partial *state;
     struct obstack *work;
{
  state->chunk_obstack = work;
  state->first = state->chunk = NULL;
  CPOOL_INIT (&state->cpool);
  BUFFER_INIT (&state->localvars);
  BUFFER_INIT (&state->bytecode);
}

static void
init_jcf_method (state, method)
     struct jcf_partial *state;
     tree method;
{
  state->current_method = method;
  state->blocks = state->last_block = NULL;
  state->linenumber_count = 0;
  state->first_lvar = state->last_lvar = NULL;
  state->lvar_count = 0;
  state->labeled_blocks = NULL;
  state->code_length = 0;
  BUFFER_RESET (&state->bytecode);
  BUFFER_RESET (&state->localvars);
  state->code_SP = 0;
  state->code_SP_max = 0;
  state->handlers = NULL;
  state->last_handler = NULL;
  state->num_handlers = 0;
  state->num_finalizers = 0;
  state->return_value_decl = NULL_TREE;
}

static void
release_jcf_state (state)
     struct jcf_partial *state;
{
  CPOOL_FINISH (&state->cpool);
  obstack_free (state->chunk_obstack, state->first);
}

/* Generate and return a list of chunks containing the class CLAS
   in the .class file representation.  The list can be written to a
   .class file using write_chunks.  Allocate chunks from obstack WORK. */

static struct chunk *
generate_classfile (clas, state)
     tree clas;
     struct jcf_partial *state;
{
  struct chunk *cpool_chunk;
  char *source_file;
  char *ptr;
  int i;
  char *fields_count_ptr;
  int fields_count = 0;
  char *methods_count_ptr;
  int methods_count = 0;
  static tree SourceFile_node = NULL_TREE;
  tree part;
  int total_supers
    = clas == object_type_node ? 0
    : TREE_VEC_LENGTH (TYPE_BINFO_BASETYPES (clas));
  
  ptr = append_chunk (NULL, 8, state);
  PUT4 (0xCafeBabe);  /* Magic number */
  PUT2 (3);  /* Minor version */
  PUT2 (45);  /* Major version */
  
  append_chunk (NULL, 0, state);
  cpool_chunk = state->chunk;

  /* Next allocate the chunk containing acces_flags through fields_counr. */
  if (clas == object_type_node)
    i = 10;
  else
    i = 8 + 2 * total_supers;
  ptr = append_chunk (NULL, i, state);
  i = get_access_flags (TYPE_NAME (clas));
  if (! (i & ACC_INTERFACE))
    i |= ACC_SUPER;
  PUT2 (i); /* acces_flags */
  i = find_class_constant (&state->cpool, clas);  PUT2 (i);  /* this_class */
  if (clas == object_type_node)
    {
      PUT2(0);  /* super_class */
      PUT2(0);  /* interfaces_count */
    }
  else
    {
      tree basetypes = TYPE_BINFO_BASETYPES (clas);
      tree base = BINFO_TYPE (TREE_VEC_ELT (basetypes, 0));
      int j = find_class_constant (&state->cpool, base);
      PUT2 (j);  /* super_class */
      PUT2 (total_supers - 1);  /* interfaces_count */
      for (i = 1;  i < total_supers;  i++)
	{
	  base = BINFO_TYPE (TREE_VEC_ELT (basetypes, i));
	  j = find_class_constant (&state->cpool, base);
	  PUT2 (j);
	}
    }
  fields_count_ptr = ptr;

  for (part = TYPE_FIELDS (clas);  part;  part = TREE_CHAIN (part))
    {
      int have_value;
      if (DECL_NAME (part) == NULL_TREE || DECL_ARTIFICIAL (part))
	continue;
      ptr = append_chunk (NULL, 8, state);
      i = get_access_flags (part);  PUT2 (i);
      i = find_utf8_constant (&state->cpool, DECL_NAME (part));  PUT2 (i);
      i = find_utf8_constant (&state->cpool, build_java_signature (TREE_TYPE (part)));
      PUT2(i);
      have_value = DECL_INITIAL (part) != NULL_TREE && FIELD_STATIC (part);
      PUT2 (have_value);  /* attributes_count */
      if (have_value)
	{
	  tree init = DECL_INITIAL (part);
	  static tree ConstantValue_node = NULL_TREE;
	  ptr = append_chunk (NULL, 8, state);
	  if (ConstantValue_node == NULL_TREE)
	    ConstantValue_node = get_identifier ("ConstantValue");
	  i = find_utf8_constant (&state->cpool, ConstantValue_node);
	  PUT2 (i);  /* attribute_name_index */
	  PUT4 (2); /* attribute_length */
	  i = find_constant_index (init, state);  PUT2 (i);
	}
      fields_count++;
    }
  ptr = fields_count_ptr;  UNSAFE_PUT2 (fields_count);

  ptr = methods_count_ptr = append_chunk (NULL, 2, state);
  PUT2 (0);

  for (part = TYPE_METHODS (clas);  part;  part = TREE_CHAIN (part))
    {
      struct jcf_block *block;
      tree function_body = DECL_FUNCTION_BODY (part);
      tree body = function_body == NULL_TREE ? NULL_TREE
	: BLOCK_EXPR_BODY (function_body);
      tree name = DECL_CONSTRUCTOR_P (part) ? init_identifier_node
	: DECL_NAME (part);
      tree type = TREE_TYPE (part);
      tree save_function = current_function_decl;
      current_function_decl = part;
      ptr = append_chunk (NULL, 8, state);
      i = get_access_flags (part);  PUT2 (i);
      i = find_utf8_constant (&state->cpool, name);  PUT2 (i);
      i = find_utf8_constant (&state->cpool, build_java_signature (type));
      PUT2 (i);
      i = (body != NULL_TREE) + (DECL_FUNCTION_THROWS (part) != NULL_TREE);
      PUT2 (i);   /* attributes_count */
      if (body != NULL_TREE)
	{
	  int code_attributes_count = 0;
	  static tree Code_node = NULL_TREE;
	  tree t;
	  char *attr_len_ptr;
	  struct jcf_handler *handler;
	  if (Code_node == NULL_TREE)
	    Code_node = get_identifier ("Code");
	  ptr = append_chunk (NULL, 14, state);
	  i = find_utf8_constant (&state->cpool, Code_node);  PUT2 (i);
	  attr_len_ptr = ptr;
	  init_jcf_method (state, part);
	  get_jcf_label_here (state);  /* Force a first block. */
	  for (t = DECL_ARGUMENTS (part);  t != NULL_TREE;  t = TREE_CHAIN (t))
	    localvar_alloc (t, state);
	  generate_bytecode_insns (body, IGNORE_TARGET, state);
	  if (CAN_COMPLETE_NORMALLY (body))
	    {
	      if (TREE_CODE (TREE_TYPE (type)) != VOID_TYPE)
		abort();
	      RESERVE (1);
	      OP1 (OPCODE_return);
	    }
	  for (t = DECL_ARGUMENTS (part);  t != NULL_TREE;  t = TREE_CHAIN (t))
	    localvar_free (t, state);
	  if (state->return_value_decl != NULL_TREE)
	    localvar_free (state->return_value_decl, state);
	  finish_jcf_block (state);
	  perform_relocations (state);

	  ptr = attr_len_ptr;
	  i = 8 + state->code_length + 4 + 8 * state->num_handlers;
	  if (state->linenumber_count > 0)
	    {
	      code_attributes_count++;
	      i += 8 + 4 * state->linenumber_count;
	    }
	  if (state->lvar_count > 0)
	    {
	      code_attributes_count++;
	      i += 8 + 10 * state->lvar_count;
	    }
	  UNSAFE_PUT4 (i); /* attribute_length */
	  UNSAFE_PUT2 (state->code_SP_max);  /* max_stack */
	  UNSAFE_PUT2 (localvar_max);  /* max_locals */
	  UNSAFE_PUT4 (state->code_length);

	  /* Emit the exception table. */
	  ptr = append_chunk (NULL, 2 + 8 * state->num_handlers, state);
	  PUT2 (state->num_handlers);  /* exception_table_length */
	  handler = state->handlers;
	  for (; handler != NULL;  handler = handler->next)
	    {
	      int type_index;
	      PUT2 (handler->start_label->pc);
	      PUT2 (handler->end_label->pc);
	      PUT2 (handler->handler_label->pc);
	      if (handler->type == NULL_TREE)
		type_index = 0;
	      else
		type_index = find_class_constant (&state->cpool,
						  handler->type);
	      PUT2 (type_index);
	    }

	  ptr = append_chunk (NULL, 2, state);
	  PUT2 (code_attributes_count);

	  /* Write the LineNumberTable attribute. */
	  if (state->linenumber_count > 0)
	    {
	      static tree LineNumberTable_node = NULL_TREE;
	      ptr = append_chunk (NULL, 8 + 4 * state->linenumber_count, state);
	      if (LineNumberTable_node == NULL_TREE)
		LineNumberTable_node = get_identifier ("LineNumberTable");
	      i = find_utf8_constant (&state->cpool, LineNumberTable_node);
	      PUT2 (i);  /* attribute_name_index */
	      i = 2+4*state->linenumber_count;  PUT4(i); /* attribute_length */
	      i = state->linenumber_count;  PUT2 (i);
	      for (block = state->blocks;  block != NULL;  block = block->next)
		{
		  int line = block->linenumber;
		  if (line > 0)
		    {
		      PUT2 (block->pc);
		      PUT2 (line);
		    }
		}
	    }

	  /* Write the LocalVariableTable attribute. */
	  if (state->lvar_count > 0)
	    {
	      static tree LocalVariableTable_node = NULL_TREE;
	      struct localvar_info *lvar = state->first_lvar;
	      ptr = append_chunk (NULL, 8 + 10 * state->lvar_count, state);
	      if (LocalVariableTable_node == NULL_TREE)
		LocalVariableTable_node = get_identifier("LocalVariableTable");
	      i = find_utf8_constant (&state->cpool, LocalVariableTable_node);
	      PUT2 (i);  /* attribute_name_index */
	      i = 2 + 10 * state->lvar_count;  PUT4 (i); /* attribute_length */
	      i = state->lvar_count;  PUT2 (i);
	      for ( ; lvar != NULL;  lvar = lvar->next)
		{
		  tree name = DECL_NAME (lvar->decl);
		  tree sig = build_java_signature (TREE_TYPE (lvar->decl));
		  i = lvar->start_label->pc;  PUT2 (i);
		  i = lvar->end_label->pc - i;  PUT2 (i);
		  i = find_utf8_constant (&state->cpool, name);  PUT2 (i);
		  i = find_utf8_constant (&state->cpool, sig);  PUT2 (i);
		  i = DECL_LOCAL_INDEX (lvar->decl);  PUT2 (i);
		}
	    }
	}
      if (DECL_FUNCTION_THROWS (part) != NULL_TREE)
	{
	  tree t = DECL_FUNCTION_THROWS (part);
	  int throws_count = list_length (t);
	  static tree Exceptions_node = NULL_TREE;
	  if (Exceptions_node == NULL_TREE)
	    Exceptions_node = get_identifier ("Exceptions");
	  ptr = append_chunk (NULL, 8 + 2 * throws_count, state);
	  i = find_utf8_constant (&state->cpool, Exceptions_node);
	  PUT2 (i);  /* attribute_name_index */ 
	  i = 2 + 2 * throws_count;  PUT4(i); /* attribute_length */ 
	  i = throws_count;  PUT2 (i); 
	  for (;  t != NULL_TREE;  t = TREE_CHAIN (t))
	    {
	      i = find_class_constant (&state->cpool, TREE_VALUE (t));
	      PUT2 (i);
	    }
	}
      methods_count++;
      current_function_decl = save_function;
    }
  ptr = methods_count_ptr;  UNSAFE_PUT2 (methods_count);

  source_file = DECL_SOURCE_FILE (TYPE_NAME (clas));
  for (ptr = source_file;  ;  ptr++)
    {
      char ch = *ptr;
      if (ch == '\0')
	break;
      if (ch == '/' || ch == '\\')
	source_file = ptr+1;
    }
  ptr = append_chunk (NULL, 10, state);
  PUT2 (1);  /* attributes_count */

  /* generate the SourceFile attribute. */
  if (SourceFile_node == NULL_TREE) 
    SourceFile_node = get_identifier ("SourceFile");
  i = find_utf8_constant (&state->cpool, SourceFile_node);
  PUT2 (i);  /* attribute_name_index */
  PUT4 (2);
  i = find_utf8_constant (&state->cpool, get_identifier (source_file));
  PUT2 (i);

  /* New finally generate the contents of the constant pool chunk. */
  i = count_constant_pool_bytes (&state->cpool);
  ptr = obstack_alloc (state->chunk_obstack, i);
  cpool_chunk->data = ptr;
  cpool_chunk->size = i;
  write_constant_pool (&state->cpool, ptr, i);
  return state->first;
}

static char *
make_class_file_name (clas)
     tree clas;
{
  const char *dname, *slash;
  char *cname, *r;
  struct stat sb;

  cname = IDENTIFIER_POINTER (identifier_subst (DECL_NAME (TYPE_NAME (clas)),
						"", '.', DIR_SEPARATOR,
						".class"));
  if (jcf_write_base_directory == NULL)
    {
      /* Make sure we put the class file into the .java file's
	 directory, and not into some subdirectory thereof.  */
      char *t;
      dname = DECL_SOURCE_FILE (TYPE_NAME (clas));
      slash = strrchr (dname, DIR_SEPARATOR);
      if (! slash)
	{
	  dname = ".";
	  slash = dname + 1;
	}
      t = strrchr (cname, DIR_SEPARATOR);
      if (t)
	cname = t + 1;
    }
  else
    {
      dname = jcf_write_base_directory;
      slash = dname + strlen (dname);
    }

  r = xmalloc (slash - dname + strlen (cname) + 2);
  strncpy (r, dname, slash - dname);
  r[slash - dname] = DIR_SEPARATOR;
  strcpy (&r[slash - dname + 1], cname);

  /* We try to make new directories when we need them.  We only do
     this for directories which "might not" exist.  For instance, we
     assume the `-d' directory exists, but we don't assume that any
     subdirectory below it exists.  It might be worthwhile to keep
     track of which directories we've created to avoid gratuitous
     stat()s.  */
  dname = r + (slash - dname) + 1;
  while (1)
    {
      cname = strchr (dname, DIR_SEPARATOR);
      if (cname == NULL)
	break;
      *cname = '\0';
      if (stat (r, &sb) == -1)
	{
	  /* Try to make it.  */
	  if (mkdir (r, 0755) == -1)
	    {
	      fatal ("failed to create directory `%s'", r);
	      free (r);
	      return NULL;
	    }
	}
      *cname = DIR_SEPARATOR;
      /* Skip consecutive separators.  */
      for (dname = cname + 1; *dname && *dname == DIR_SEPARATOR; ++dname)
	;
    }

  return r;
}

/* Write out the contens of a class (RECORD_TYPE) CLAS, as a .class file.
   The output .class file name is make_class_file_name(CLAS). */

void
write_classfile (clas)
     tree clas;
{
  struct obstack *work = &temporary_obstack;
  struct jcf_partial state[1];
  char *class_file_name = make_class_file_name (clas);
  struct chunk *chunks;

  if (class_file_name != NULL)
    {
      FILE* stream = fopen (class_file_name, "wb");
      if (stream == NULL)
	fatal ("failed to open `%s' for writing", class_file_name);
      jcf_dependency_add_target (class_file_name);
      init_jcf_state (state, work);
      chunks = generate_classfile (clas, state);
      write_chunks (stream, chunks);
      if (fclose (stream))
	fatal ("failed to close after writing `%s'", class_file_name);
      free (class_file_name);
    }
  release_jcf_state (state);
}

/* TODO:
   string concatenation
   synchronized statement
   */
