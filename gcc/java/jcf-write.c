/* Write out a Java(TM) class file.
   Copyright (C) 1998 Free Software Foundation, Inc.

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
#include "tree.h"
#include "java-tree.h"
#include "jcf.h"
#include "obstack.h"
#undef AND
#include "rtl.h"
#include "java-opcodes.h"
#include "parse.h" /* for BLOCK_EXPR_BODY */
#include "buffer.h"

extern struct obstack temporary_obstack;

/* Make sure bytecode.data is big enough for at least N more bytes. */

#define RESERVE(N) \
  do { if (state->bytecode.ptr + (N) > state->bytecode.limit) \
    buffer_grow (&state->bytecode, N); } while (0)

/* Add a 1-byte instruction/operand I to bytecode.data,
   assuming space has already been RESERVE'd. */

#define OP1(I) (*state->bytecode.ptr++ = (I))

/* Like OP1, but I is a 2-byte big endian integer. */

#define OP2(I) \
  do { int _i = (I);  OP1 (_i >> 8);  OP1 (_i); } while (0)

/* Like OP1, but I is a 4-byte big endian integer. */

#define OP4(I) \
  do { int _i = (I);  OP1 (_i >> 24);  OP1 (_i >> 16); \
       OP1 (_i >> 8); OP1 (_i); } while (0)

/* The current stack size (stack pointer) in the current method. */

int code_SP = 0;

/* The largest extent of stack size (stack pointer) in the current method. */

int code_SP_max = 0;

CPool *code_cpool;

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

/* Each "block" represents a label plus the bytecode instructions following.
   There may be branches out of the block, but no incoming jumps, except
   to the beginning of the block. */

struct jcf_block
{
  /* For blocks that that are defined, the next block (in pc order).
     For blocks that are the not-yet-defined end label of a LABELED_BLOCK_EXPR,
     this is the next (outer) such end label, in a stack heaed by
     labeled_blocks in jcf_partial. */
  struct jcf_block *next;

  /* Until perform_relocations is finished, this is the maximum possible
     value of the bytecode offset at the begnning of this block.
     After perform_relocations, it is the actual offset (pc). */
  int pc;

  int linenumber;

  struct chunk *chunk;

  union {
    /* Set of relocations (in reverse offset order) for this block. */
    struct jcf_relocation *relocations;

    /* If this block is that of the not-yet-defined end label of
       a LABELED_BLOCK_EXPR, where LABELED_BLOCK is that LABELED_BLOCK_EXPR. */
    tree labeled_block;
  } u;
};

struct jcf_relocation
{
  /* Next relocation for the current jcf_block. */
  struct jcf_relocation *next;

  /* The (byte) offset within the current block that needs to be relocated. */
  int offset;

  /* 0 if offset is a 4-byte relative offset.
     -1 if offset is a 2-byte relative offset.
     < 0 if offset is the address of an instruction with a 2-byte offset
     that does not have a corresponding 4-byte offset version, in which
     case the absolute value of kind is the inverted opcode.
     > 0 if offset is the address of an instruction (such as jsr) with a
     2-byte offset that does have a corresponding 4-byte offset version,
     in which case kind is the opcode of the 4-byte version (such as jsr_w). */
  int kind;

  /* The label the relocation wants to actually transfer to. */
  struct jcf_block *label;
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
};

static void generate_bytecode_insns PROTO ((tree, int, struct jcf_partial *));

/* Utility macros for appending (big-endian) data to a buffer.
   We assume a local variable 'ptr' points into where we want to
   write next, and we assume enoygh space has been allocated. */

#define PUT1(X)  (*ptr++ = (X))
#define PUT2(X)  (PUT1((X) >> 8), PUT1((X) & 0xFF))
#define PUT4(X)  (PUT2((X) >> 16), PUT2((X) & 0xFFFF))
#define PUTN(P, N)  (bcopy(P, ptr, N), ptr += (N))


/* Allocate a new chunk on obstack WORK, and link it in after LAST.
   Set the data and size fields to DATA and SIZE, respectively.
   However, if DATA is NULL and SIZE>0, allocate a buffer as well. */

struct chunk *
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

unsigned char *
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

void
append_chunk_copy (data, size, state)
     unsigned char *data;
     int size;
     struct jcf_partial *state;
{
  unsigned char *ptr = append_chunk (NULL, size, state);
  bcopy (data, ptr, size);
}

struct jcf_block *
gen_jcf_label (state)
     struct jcf_partial *state;
{
  struct jcf_block *block = (struct jcf_block *)
    obstack_alloc (state->chunk_obstack, sizeof (struct jcf_block));
  block->next =	NULL;
  block->linenumber = -1;
  block->pc = -1;
  return block;
}

void
finish_jcf_block (state)
     struct jcf_partial *state;
{
  struct jcf_block *block = state->last_block;
  struct jcf_relocation *reloc;
  int pc = state->code_length;
  append_chunk_copy (state->bytecode.data, BUFFER_LENGTH (&state->bytecode),
		     state);
  BUFFER_RESET (&state->bytecode);
  block->chunk = state->chunk;

  /* Calculate code_length to the maximum value it can have. */
  pc += block->chunk->size;
  for (reloc = block->u.relocations;  reloc != NULL;  reloc = reloc->next)
    {
      int kind = reloc->kind;
      if (kind > 0)
	pc += 2; /* 2-byte offset may grow to 4-byte offset */
      else if (kind < -1)
	pc += 5; /* May need to add a goto_w. */
    }
  state->code_length = pc;
}

void
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

struct jcf_block *
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

void
put_linenumber (line, state)
     int line;
     struct jcf_partial *state;
{
  (get_jcf_label_here (state))->linenumber = line;
  state->linenumber_count++;
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

int
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

  if (DECL_NAME (decl) != NULL_TREE)
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

int
localvar_free (decl, state)
     tree decl;     
     struct jcf_partial *state;
{
  struct jcf_block *end_label = get_jcf_label_here (state);
  int index = DECL_LOCAL_INDEX (decl);
  register struct localvar_info **ptr = &localvar_buffer [index];
  register struct localvar_info *info = *ptr;
  int wide = TYPE_IS_WIDE (TREE_TYPE (decl));
  int i;

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

int
get_access_flags (decl)
    tree decl;
{
  int flags = 0;
  int isfield = TREE_CODE (decl) == FIELD_DECL || TREE_CODE (decl) == VAR_DECL;
  if (CLASS_PUBLIC (decl))  /* same as FIELD_PUBLIC and METHOD_PUBLIC */
    flags |= ACC_PUBLIC;
  if (CLASS_FINAL (decl))  /* same as FIELD_FINAL and METHOD_FINAL */
    flags |= ACC_PUBLIC;
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
      if (METHOD_FINAL (decl))
	flags |= ACC_FINAL;
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

void
write_chunks (stream, chunks)
     FILE* stream;
     struct chunk *chunks;
{
  for (;  chunks != NULL;  chunks = chunks->next)
    fwrite (chunks->data, chunks->size, 1, stream);
}

static void
push_constant1 (index, state)
     int index;
     struct jcf_partial *state;
{
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

static void
push_constant2 (index, state)
     int index;
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
      NOTE_PUSH (1);
    }
  else
    {
      i = find_constant1 (&state->cpool, CONSTANT_Integer, i & 0xFFFFFFFF);
      push_constant1 (i);
    }
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
#if 0
    else if ((jlong) (jint) i == i)
      {
        push_int_const ((jint) i, state);
        RESERVE (1);
        OP1 (OPCODE_i2l);
      }
#endif
  else
    {
      HOST_WIDE_INT w1, w2;
      lshift_double (lo, hi, -32, 64, &w1, &w2, 1);
      hi = find_constant1 (&state->cpool, CONSTANT_Long,
			   w1 & 0xFFFFFFFF, lo & 0xFFFFFFFF);
      push_constant2 (hi);
    }
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

int
adjust_typed_op (type)
     tree type;
{
  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
    case RECORD_TYPE:   return 4;
    case BOOLEAN_TYPE:
      return TYPE_PRECISION (type) == 32 ? 0 : 5;
    case CHAR_TYPE:
      return TYPE_PRECISION (type) == 32 ? 0 : 6;
    case INTEGER_TYPE:
      switch (TYPE_PRECISION (type))
	{
	case  8:       return 5;
	case 16:       return 7;
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
     int value;
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
     tree var;
     struct jcf_partial *state;
{
  tree type = TREE_TYPE (var);
  int kind = adjust_typed_op (type);
  int index = DECL_LOCAL_INDEX (var);
  if (index <= 3)
    {
      RESERVE (1);
      OP1 (opcode + 5 + 4 * kind + index);    /* [ilfda]{load,store}_[0123] */
    }
  else
    maybe_wide (opcode + kind, index);  /* [ilfda]{load,store} */
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

/* Emit a conditional jump to TARGET with a 2-byte relative jump offset
   The opcode is OPCODE, the inverted opcode is INV_OPCODE. */

static void
emit_if (target, opcode, inv_opcode, state)
     struct jcf_block *target;
     int opcode, inv_opcode;
     struct jcf_partial *state;
{
  struct jcf_relocation *reloc = (struct jcf_relocation *)
    obstack_alloc (state->chunk_obstack, sizeof (struct jcf_relocation));
  struct jcf_block *block = state->last_block;
  reloc->next = block->u.relocations;
  block->u.relocations = reloc;
  OP1 (opcode);
  reloc->offset = BUFFER_LENGTH (&state->bytecode);
  OP2 (1); // 1 byte from reloc back to start of instruction.
  reloc->kind = - inv_opcode;
  reloc->label = target;
}

static void
emit_goto_or_jsr (target, opcode, opcode_w, state)
     struct jcf_block *target;
     int opcode, opcode_w;
     struct jcf_partial *state;
{
  struct jcf_relocation *reloc = (struct jcf_relocation *)
    obstack_alloc (state->chunk_obstack, sizeof (struct jcf_relocation));
  struct jcf_block *block = state->last_block;
  reloc->next = block->u.relocations;
  block->u.relocations = reloc;
  OP1 (opcode);
  reloc->offset = BUFFER_LENGTH (&state->bytecode);
  OP2 (1); // 1 byte from reloc back to start of instruction.
  reloc->kind = opcode_w;
  reloc->label = target;
}

static void
emit_goto (target, state)
     struct jcf_block *target;
     struct jcf_partial *state;
{
  emit_goto_or_jsr (target, OPCODE_goto, OPCODE_goto_w, state);
}

static void
emit_jsr (target, state)
     struct jcf_block *target;
     struct jcf_partial *state;
{
  emit_goto_or_jsr (target, OPCODE_jsr, OPCODE_jsr_w, state);
}

/* Generate code to evaluate EXP.  If the result is true,
   branch to TRUE_LABEL; otherwise, branch to FALSE_LABEL.
   TRUE_BRANCH_FIRST is a code geneation hint that the
   TRUE_LABEL may follow right after this. (The idea is that we
   may be able to optimize away GOTO TRUE_LABEL; TRUE_LABEL:) */

void
generate_bytecode_conditional (exp, true_label, false_label,
			       true_branch_first, state)
     tree exp;
     struct jcf_block *true_label;
     struct jcf_block *false_label;
     int true_branch_first;
     struct jcf_partial *state;
{
  int kind;
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
	case POINTER_TYPE:  case RECORD_TYPE:
	  switch (TREE_CODE (exp))
	    {
	    case EQ_EXPR:  op = OPCODE_if_acmpeq;  break;
	    case NE_EXPR:  op = OPCODE_if_acmpne;  break;
	    default:  abort();
	    }
	  if (integer_zerop (exp1) || integer_zerop (exp0))
	    {
	      generate_bytecode_insns (integer_zerop (exp1) ? exp0 : exp0,
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
	  fatal ("float comparison not implemented");
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
    fatal ("inetrnal error - SP mismatch");
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
	  for (local = BLOCK_EXPR_DECLS (exp); local; )
	    {
	      tree next = TREE_CHAIN (local);
	      localvar_alloc (local, state);
	      local = next;
	    }
	  generate_bytecode_insns (BLOCK_EXPR_BODY (exp), target, state);
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
	int saved_lineno = lineno;
	input_filename = EXPR_WFL_FILENAME (exp);
	lineno = EXPR_WFL_LINENO (exp);
	if (EXPR_WFL_EMIT_LINE_NOTE (exp))
	  put_linenumber (EXPR_WFL_LINENO (exp), state);
	generate_bytecode_insns (EXPR_WFL_NODE (exp), target, state);
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
    case INDIRECT_REF:
      generate_bytecode_insns (TREE_OPERAND (exp, 0), target, state);
      break;
    case ARRAY_REF:
      generate_bytecode_insns (TREE_OPERAND (exp, 0), target, state);
      generate_bytecode_insns (TREE_OPERAND (exp, 1), target, state);
      if (target != IGNORE_TARGET)
	{
	  jopcode = OPCODE_iaload + adjust_typed_op (type);
	  RESERVE(1);
	  OP1 (jopcode);
	  NOTE_POP (2);
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
	emit_goto (end_label, state);
	define_jcf_label (else_label, state);
	generate_bytecode_insns (TREE_OPERAND (exp, 2), target, state);
	define_jcf_label (end_label, state);
      }
      break;
    case RETURN_EXPR:
      if (!TREE_OPERAND (exp, 0))
	op = OPCODE_return;
      else
	{
	  exp = TREE_OPERAND (exp, 0);
	  if (TREE_CODE (exp) != MODIFY_EXPR)
	    abort ();
	  exp = TREE_OPERAND (exp, 1);
	  op = OPCODE_ireturn + adjust_typed_op (TREE_TYPE (exp));
	  generate_bytecode_insns (exp, STACK_TARGET, state);
	}
      RESERVE (1);
      OP1 (op);
      break;
    case LABELED_BLOCK_EXPR:
      {
	struct jcf_block *end_label = gen_jcf_label (state);
	end_label->next = state->labeled_blocks;
	state->labeled_blocks = end_label;
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
	  if (target != IGNORE_TARGET)
	    {
	      if (! post_op)
		emit_load (exp, state);
	      NOTE_PUSH (1);
	    }
	  break;
	}
      if (TREE_CODE (exp) == COMPONENT_REF)
	{
	  generate_bytecode_insns (TREE_OPERAND (exp, 0), STACK_TARGET, state);
	  emit_dup (1, 0, state);
	  /* Stack:  ..., objectref, objectref. */
	  field_op (TREE_OPERAND (exp, 1), OPCODE_getstatic, state);
	  NOTE_PUSH (size);
	  /* Stack:  ..., objectref, oldvalue. */
	  offset = 1;
	}
      else if (TREE_CODE (exp) == ARRAY_REF)
	{
	  generate_bytecode_insns (TREE_OPERAND (exp, 0), STACK_TARGET, state);
	  generate_bytecode_insns (TREE_OPERAND (exp, 1), STACK_TARGET, state);
	  emit_dup (2, 0, state);
	  /* Stack:  ..., array, index, array, index. */
	  jopcode = OPCODE_iaload + adjust_typed_op (TREE_TYPE (exp));
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
      push_int_const (value, state); /* FIXME - assumes int! */
      NOTE_PUSH (1);
      emit_binop (OPCODE_iadd + adjust_typed_op (type), type, state);
      if (target != IGNORE_TARGET && ! post_op)
	emit_dup (size, offset, state);
      /* Stack:  ..., [result,] newvalue. */
      goto finish_assignment;

    case MODIFY_EXPR:
      {
	tree lhs = TREE_OPERAND (exp, 0);
	tree rhs = TREE_OPERAND (exp, 1);

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
	  generate_bytecode_insns (TREE_OPERAND (lhs, 0), STACK_TARGET, state);
	else if (TREE_CODE (lhs) == ARRAY_REF)
	  {
	    generate_bytecode_insns (TREE_OPERAND(lhs, 0), STACK_TARGET, state);
	    generate_bytecode_insns (TREE_OPERAND(lhs, 1), STACK_TARGET, state);
	  }
	generate_bytecode_insns (rhs, STACK_TARGET, state);
	if (target != IGNORE_TARGET)
	  emit_dup (TYPE_IS_WIDE (type) ? 2 : 1 , 1, state);
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
		    FIELD_STATIC (field) ? OPCODE_putstatic
		    : OPCODE_putfield,
		    state);

	  NOTE_PUSH (TYPE_IS_WIDE (TREE_TYPE (field)) ? 2 : 1);
	}
      else if (TREE_CODE (exp) == VAR_DECL
	       || TREE_CODE (exp) == PARM_DECL)
	{
	  if (FIELD_STATIC (exp))
	    {
	      field_op (exp, OPCODE_putstatic, state);
	      NOTE_PUSH (TYPE_IS_WIDE (TREE_TYPE (exp)) ? 2 : 1);
	    }
	  else
	    emit_store (exp, state);
	}
      else if (TREE_CODE (exp) == ARRAY_REF)
	{
	  NOTE_POP (2);
	  jopcode = OPCODE_iastore + adjust_typed_op (TREE_TYPE (exp));
	  RESERVE(1);
	  OP1 (jopcode);
	  NOTE_PUSH (TYPE_IS_WIDE (TREE_TYPE (exp)) ? 2 : 1);
	}
      else
	fatal ("internal error (bad lhs to MODIFY_EXPR)");
      break;
    case PLUS_EXPR:
      jopcode = OPCODE_iadd + adjust_typed_op (type);
      goto binop;
    case MINUS_EXPR:
      jopcode = OPCODE_isub + adjust_typed_op (type);
      goto binop;
    case MULT_EXPR:
      jopcode = OPCODE_imul + adjust_typed_op (type);
      goto binop;
    case TRUNC_DIV_EXPR:
    case RDIV_EXPR:
      jopcode = OPCODE_idiv + adjust_typed_op (type);
      goto binop;
    binop:
      generate_bytecode_insns (TREE_OPERAND (exp, 0), target, state);
      generate_bytecode_insns (TREE_OPERAND (exp, 1), target, state);
      if (target == STACK_TARGET)
	emit_binop (jopcode, type, state);
      break;
    case CALL_EXPR:
      {
	tree t;
	int save_SP = state->code_SP;
	for (t = TREE_OPERAND (exp, 1);  t != NULL_TREE;  t = TREE_CHAIN (t))
	  {
	    generate_bytecode_insns (TREE_VALUE (t), STACK_TARGET, state);
	  }
	t = TREE_OPERAND (exp, 0);
	state->code_SP = save_SP;
	if (TREE_CODE (t) == FUNCTION_DECL)
	  {
	    int index = find_methodref_index (&state->cpool, t);
	    RESERVE (3);
	    if (DECL_CONSTRUCTOR_P (t))
	      OP1 (OPCODE_invokespecial);
	    else if (METHOD_STATIC (t))
	      OP1 (OPCODE_invokestatic);
	    else
	      OP1 (OPCODE_invokevirtual);
	    OP2 (index);
	    t = TREE_TYPE (TREE_TYPE (t));
	    if (TREE_CODE (t) != VOID_TYPE)
	      {
		int size = TYPE_IS_WIDE (t) ? 2 : 1;
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

void
perform_relocations (state)
     struct jcf_partial *state;
{
  struct jcf_block *block;
  struct jcf_relocation *reloc;
  int pc;
  int shrink;

  /* Figure out the actual locations of each block. */
  pc = 0;
  shrink = 0;
  for (block = state->blocks;  block != NULL;  block = block->next)
    {
      int block_size = block->chunk->size;

      block->pc = pc;

      /* Optimize GOTO L; L: by getting rid of the redundant goto.
	 Assumes relocations are in reverse order. */
      reloc = block->u.relocations;
      while (reloc != NULL
	     && reloc->label->pc == block->next->pc
	     && reloc->offset + 2 == block_size
	     && reloc->kind == OPCODE_goto_w)
	{
	  reloc = reloc->next;
	  block->u.relocations = reloc;
	  block->chunk->size -= 3;
	  block_size -= 3;
	  shrink += 3;
	}

      for (reloc = block->u.relocations;  reloc != NULL;  reloc = reloc->next)
	{
	  if (reloc->kind < -1 || reloc->kind > 0)
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
      struct chunk *chunk = block->chunk;
      int old_size = chunk->size;
      int next_pc = block->next == NULL ? pc : block->next->pc;
      int new_size = next_pc - block->pc;
      int offset = 0;
      unsigned char *new_ptr;
      unsigned char *old_buffer = chunk->data;
      unsigned char *old_ptr = old_buffer + old_size;
      int new_end = new_size;
      if (new_size != old_size)
	{
	  chunk->data = (unsigned char *)
	    obstack_alloc (state->chunk_obstack, new_size);
	}
      new_ptr = chunk->data + new_size;

      /* We do the relocations from back to front, because
	 thre relocations are in reverse order. */
      for (reloc = block->u.relocations; ; reloc = reloc->next)
	{
	  /* Lower old index of piece to be copied with no relocation. */
	  int start = reloc == NULL ? 0
	    : reloc->kind == 0 ? reloc->offset + 4
	    : reloc->offset + 2;
	  int32 value;
	  int new_offset;
	  int n = (old_ptr - old_buffer) - start;
	  new_ptr -= n;
	  old_ptr -= n;
	  if (n > 0)
	    bcopy (old_ptr, new_ptr, n);
	  if (old_ptr == old_buffer)
	    break;

	  if (reloc->kind == 0)
	    {
	      old_ptr -= 4;
	      value = GET_u4 (old_ptr);
	    }
	  else
	    {
	      old_ptr -= 2;
	      value = GET_u2 (old_ptr);
	    }
	  new_offset = new_ptr - chunk->data - (reloc->kind == -1 ? 2 : 4);
	  value += reloc->label->pc - (block->pc + new_offset);
	  *--new_ptr = (unsigned char) value;  value >>= 8;
	  *--new_ptr = (unsigned char) value;  value >>= 8;
	  if (reloc->kind != -1)
	    {
	      *--new_ptr = (unsigned char) value;  value >>= 8;
	      *--new_ptr = (unsigned char) value;
	    }
	  if (reloc->kind > 0)
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
    }
  state->code_length = pc;
}

void
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

void
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
}

void
release_jcf_state (state)
     struct jcf_partial *state;
{
  CPOOL_FINISH (&state->cpool);
  obstack_free (state->chunk_obstack, state->first);
}

/* Generate and return a list of chunks containing the class CLAS
   in the .class file representation.  The list can be written to a
   .class file using write_chunks.  Allocate chunks from obstack WORK. */

struct chunk *
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
  i = get_access_flags (TYPE_NAME (clas));  PUT2 (i); /* acces_flags */
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
      if (DECL_NAME (part) == NULL_TREE)
	continue;
      ptr = append_chunk (NULL, 8, state);
      i = get_access_flags (part);  PUT2 (i);
      i = find_utf8_constant (&state->cpool, DECL_NAME (part));  PUT2 (i);
      i = find_utf8_constant (&state->cpool, build_java_signature (TREE_TYPE (part)));
      PUT2(i);
      PUT2 (0);  /* attributes_count */
      /* FIXME - emit ConstantValue attribute when appropriate */
      fields_count++;
    }
  ptr = fields_count_ptr;  PUT2 (fields_count);

  ptr = methods_count_ptr = append_chunk (NULL, 2, state);
  PUT2 (0);

  for (part = TYPE_METHODS (clas);  part;  part = TREE_CHAIN (part))
    {
      struct jcf_block *block;
      tree body = BLOCK_EXPR_BODY (DECL_FUNCTION_BODY (part));
      tree name = DECL_CONSTRUCTOR_P (part) ? init_identifier_node
	: DECL_NAME (part);
      tree type = TREE_TYPE (part);
      ptr = append_chunk (NULL, 8, state);
      i = get_access_flags (part);  PUT2 (i);
      i = find_utf8_constant (&state->cpool, name);  PUT2 (i);
      i = find_utf8_constant (&state->cpool, build_java_signature (type));
      PUT2 (i);
      PUT2 (body != NULL_TREE ? 1 : 0);   /* attributes_count */
      if (body != NULL_TREE)
	{
	  int code_attributes_count = 0;
	  static tree Code_node = NULL_TREE;
	  tree t;
	  char *attr_len_ptr;
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
	  for (t = DECL_ARGUMENTS (part);  t != NULL_TREE;  t = TREE_CHAIN (t))
	    localvar_free (t, state);
	  finish_jcf_block (state);
	  perform_relocations (state);

	  ptr = attr_len_ptr;
	  i = 8 + state->code_length + 4;
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
	  PUT4 (i); /* attribute_length */
	  PUT2 (state->code_SP_max);  /* max_stack */
	  PUT2 (localvar_max);  /* max_locals */
	  PUT4 (state->code_length);
	  ptr = append_chunk (NULL, 4, state);
	  PUT2 (0);  /* exception_table_length */
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
      methods_count++;
    }
  ptr = methods_count_ptr;  PUT2 (methods_count);

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

char*
make_class_file_name (clas)
     tree clas;
{
  /* Should prepend an output directly, but need an option to specify it. */
  return IDENTIFIER_POINTER (identifier_subst (DECL_NAME (TYPE_NAME (clas)),
					       "", '.', '/', ".class"));
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
  FILE* stream = fopen (class_file_name, "wb");
  if (stream == NULL)
    fatal ("failed to open `%s' for writing", class_file_name);
  init_jcf_state (state, work);
  chunks = generate_classfile (clas, state);
  write_chunks (stream, chunks);
  if (fclose (stream))
    fatal ("failed to close after writing `%s'", class_file_name);
  release_jcf_state (state);
}
