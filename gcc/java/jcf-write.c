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

/* The buffer allocated for bytecode for the current method. */

struct buffer bytecode = NULL_BUFFER;

/* Make sure bytecode.data is big enough for at least N more bytes. */

#define RESERVE(N) \
  do { if (bytecode.ptr + (N) > bytecode.limit) buffer_grow (&bytecode, N); } while (0)

/* Add a 1-byte instruction/operand I to bytecode.data,
   assuming space has already been RESERVE'd. */

#define OP1(I) (*bytecode.ptr++ = (I))

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
  do { code_SP += (I); if (code_SP > code_SP_max) code_SP_max = code_SP; } while (0)

/* Macro to call each time we pop I words from the JVM stack. */

#define NOTE_POP(I) \
  do { code_SP -= (I); if (code_SP < 0) abort(); } while (0)

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

/* Utility macros for appending (big-endian) data to a buffer.
   We assume a local variable 'ptr' points into where we want to
   write next, and we assume enoygh space has been allocated. */

#define PUT1(X)  (*ptr++ = (X))
#define PUT2(X)  (PUT1((X) >> 8), PUT1((X) & 0xFF))
#define PUT4(X)  (PUT2((X) >> 16), PUT2((X) & 0xFFFF))
#define PUTN(P, N)  (bcopy(P, ptr, N), ptr += (N))


/* A buffer for storing line number entries for the current method. */
struct buffer linenumbers = NULL_BUFFER;

/* Append a line number entry for the given PC and LINE into
   linenumbers.data.  This will later before a LineNumberTable attribute. */

void
put_linenumber (pc, line)
     int pc, line;
{
  register unsigned char *ptr;
  if (linenumbers.ptr == linenumbers.limit)
    buffer_grow (&linenumbers, 4);
  ptr = linenumbers.ptr;
  PUT2 (pc);
  PUT2 (line);
  linenumbers.ptr = ptr;
}

/* The index of jvm local variable allocated for this DECL.
   This is assign when generating .class files;
   contrast DECL_LOCAL_SLOT_NUMBER whcih is set when *reading* a .class file.
   (We don't allocate DECL_LANG_SPECIFIC for locals from Java sourc code.) */

#define DECL_LOCAL_INDEX(DECL) DECL_ALIGN(DECL)

struct localvar_info
{
  tree decl;

  int start_pc;

  /* Offset in LocalVariableTable. */
  int debug_offset;
};

struct buffer localvars = NULL_BUFFER;

#define localvar_buffer ((struct localvar_info*) localvars.data)
#define localvar_max ((struct localvar_info*) localvars.ptr - localvar_buffer)

/* A buffer for storing LocalVariableTable entries entries. */

struct buffer localvartable = NULL_BUFFER;

int
localvar_alloc (decl, start_pc)
     tree decl;
     int start_pc;
{
  int wide = TYPE_IS_WIDE (TREE_TYPE (decl));
  int index;
  register struct localvar_info *info = (struct localvar_info*)localvars.data;
  register struct localvar_info *limit = (struct localvar_info*)localvars.ptr;
  for (index = 0;  info < limit;  index++, info++)
    {
      if (info->decl == NULL_TREE
	  && (! wide || (info+1)->decl == NULL_TREE))
	break;
    }
  if (info == limit)
    {
      buffer_grow (&localvars, sizeof (struct localvar_info));
      info = (struct localvar_info*)localvars.data + index;
      localvars.ptr = (unsigned char *) (info + 1 + wide);
    }
  info->decl = decl;
  if (wide)
    (info+1)->decl = TYPE_SECOND;
  DECL_LOCAL_INDEX (decl) = index;
  info->start_pc = start_pc;

  if (DECL_NAME (decl) != NULL_TREE)
    {
      /* Generate debugging info. */
      int i;
      register unsigned char *ptr;
      buffer_grow (&localvartable, 10);
      ptr = localvartable.ptr;
      info->debug_offset = ptr - localvartable.data;
      PUT2 (start_pc);
      PUT2 (0);  /* length - fill in later */
      i = find_utf8_constant (code_cpool, DECL_NAME (decl));
      PUT2 (i); /* name_index*/
      i = find_utf8_constant (code_cpool,
			      build_java_signature (TREE_TYPE (decl)));
      PUT2 (i);  /* descriptor_index */
      PUT2 (index);
      localvartable.ptr = ptr;
    }
  else
    info->debug_offset = -1;
}

int
localvar_free (decl, end_pc)
     tree decl;
     int end_pc;
{
  register unsigned char *ptr;
  int index = DECL_LOCAL_INDEX (decl);
  register struct localvar_info *info = &localvar_buffer [index];
  int wide = TYPE_IS_WIDE (TREE_TYPE (decl));
  int i;

  i = info->debug_offset;
  if (i >= 0)
    {
      register unsigned char *ptr;
      /* Point to length field of local_variable_table. */
      ptr = localvartable.data + i + 2;
      i = end_pc - info->start_pc;
      PUT2 (i);
    }

  if (info->decl != decl)
    abort ();
  info->decl = NULL_TREE;
  if (wide)
    {
      info++;
      if (info->decl != TYPE_SECOND)
	abort ();
      info->decl = NULL_TREE;
    }

}


#define STACK_TARGET 1
#define IGNORE_TARGET 2

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
  last->next = chunk;
  return chunk;
}

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

void
push_constant1 (index)
     int index;
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

void
push_constant2 (index)
     int index;
{
  RESERVE (3);
  OP1 (OPCODE_ldc2_w);
  OP2 (index);
}

void
push_int_const (i)
     HOST_WIDE_INT i;
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
      i = find_constant1 (code_cpool, CONSTANT_Integer, i & 0xFFFFFFFF);
      push_constant1 (i);
    }
}

void
push_long_const (lo, hi)
     HOST_WIDE_INT lo, hi;
{
  if (hi == 0 && lo >= 0 && lo <= 1)
    {
      RESERVE(1);
      OP1(OPCODE_lconst_0 + lo);
    }
#if 0
    else if ((jlong) (jint) i == i)
      {
        push_int_const ((jint) i);
        RESERVE (1);
        OP1 (OPCODE_i2l);
      }
#endif
  else
    {
      HOST_WIDE_INT w1, w2;
      lshift_double (lo, hi, -32, 64, &w1, &w2, 1);
      hi = find_constant1 (code_cpool, CONSTANT_Long,
			   w1 & 0xFFFFFFFF, lo & 0xFFFFFFFF);
      push_constant2 (hi);
    }
}

void
field_op (field, opcode)
     tree field;
     int opcode;
{
  int index = find_fieldref_index (code_cpool, field);
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
    case BOOLEAN_TYPE:  return 5;
    case CHAR_TYPE:     return 6;
    case POINTER_TYPE:
    case RECORD_TYPE:   return 4;
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

void
maybe_wide (opcode, index)
     int opcode, index;
{
  if (index >= 256)
    {
      RESERVE (4);
      OP1 (196); /* wide */
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

#define PC BUFFER_LENGTH(&bytecode)

/* Generate byetcode for sub-expression EXP of METHOD.
   TARGET is one of STACK_TARGET or IGNORE_TARGET. */

void
generate_bytecode_insns (method, exp, target)
     tree method;
     tree exp;
     int target;
{
  rtx value;
  tree type = TREE_TYPE (exp);
  enum java_opcode jopcode;
  int op;
  switch (TREE_CODE (exp))
    {
    case BLOCK:
      if (BLOCK_EXPR_BODY (exp))
	{
	  tree local;
	  for (local = BLOCK_EXPR_DECLS (exp); local; )
	    {
	      tree next = TREE_CHAIN (local);
	      localvar_alloc (local, PC);
	      local = next;
	    }
	  generate_bytecode_insns (method, BLOCK_EXPR_BODY (exp), target);
	  for (local = BLOCK_EXPR_DECLS (exp); local; )
	    {
	      tree next = TREE_CHAIN (local);
	      localvar_free (local, PC);
	      local = next;
	    }
	}
      break;
      case COMPOUND_EXPR:	
	generate_bytecode_insns (method, TREE_OPERAND (exp, 0), IGNORE_TARGET);
	generate_bytecode_insns (method, TREE_OPERAND (exp, 1), target);
      break;
    case EXPR_WITH_FILE_LOCATION:
      {
	char *saved_input_filename = input_filename;
	int saved_lineno = lineno;
	input_filename = EXPR_WFL_FILENAME (exp);
	lineno = EXPR_WFL_LINENO (exp);
	if (EXPR_WFL_EMIT_LINE_NOTE (exp))
	  put_linenumber (PC, EXPR_WFL_LINENO (exp));
	generate_bytecode_insns (method, EXPR_WFL_NODE (exp), target);
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
	  push_int_const (TREE_INT_CST_LOW (exp));
	  NOTE_PUSH (1);
	}
      else
	{
	  push_long_const (TREE_INT_CST_LOW (exp), TREE_INT_CST_HIGH (exp));
	  NOTE_PUSH (2);
	}
      break;
    case VAR_DECL:
      if (TREE_STATIC (exp))
	{
	  field_op (exp, OPCODE_getstatic);
	  break;
	}
      /* ... fall through ... */
    case PARM_DECL:
      {
	int kind = adjust_typed_op (type);
	int index = DECL_LOCAL_INDEX (exp);
	if (index <= 3)
	  {
	    RESERVE (1);
	    OP1 (26 + 4 * kind + index);    /* [ilfda]load_[0123] */
	  }
	else
	  maybe_wide (21 + kind, index);  /* [ilfda]load */
      }
      break;
    case INDIRECT_REF:
      generate_bytecode_insns (method, TREE_OPERAND (exp, 0), target);
      break;
    case ARRAY_REF:
      generate_bytecode_insns (method, TREE_OPERAND (exp, 0), target);
      generate_bytecode_insns (method, TREE_OPERAND (exp, 1), target);
      if (target != IGNORE_TARGET)
	{
	  jopcode = OPCODE_iaload + adjust_typed_op (type);
	  RESERVE(1);
	  OP1 (jopcode);
	}
      break;
    case COMPONENT_REF:
      {
	tree obj = TREE_OPERAND (exp, 0);
	tree field = TREE_OPERAND (exp, 1);
	int is_static = FIELD_STATIC (field);
	generate_bytecode_insns (method, obj,
				 is_static ? IGNORE_TARGET : target);
	if (target != IGNORE_TARGET)
	  {
	    if (DECL_NAME (field) == length_identifier_node && !is_static
		&& TYPE_ARRAY_P (TREE_TYPE (obj)))
	      {
		RESERVE (1);
		OP1 (OPCODE_arraylength);
	      }
	    else
	      field_op (field, is_static ? OPCODE_getstatic : OPCODE_getfield);
	  }
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
	  generate_bytecode_insns (method, exp, STACK_TARGET);
	}
      RESERVE (1);
      OP1 (op);
      break;
    case MODIFY_EXPR:
      {
	tree lhs = TREE_OPERAND (exp, 0);
	tree rhs = TREE_OPERAND (exp, 1);
	HOST_WIDE_INT value;
#if 0
	if (TREE_CODE (rhs) == PLUS_EXPR
	    && TREE_CODE (lhs) == VAR_DECL
	    /* && FIXME lhs is a local variable */
	    && TYPE_MODE (TREE)TYPE (lhs) == SImode /* ??? */
	    && TREE_OPERAND (rhs, 0) == lhs
	    && TREE_CODE (TREE_OPERAND (rhs, 1)) == INTEGER_CST
	    /* or vice versa FIXME */
	    && (value = TREE_INT_CST_LOW (TREE_OPERAND (rhs, 1)),
		(value >= -32768 && value <= 32767)))
	  {
	    emit_insn (gen_rtx (SET, SImode,
				DECL_RTL (lhs),
				gen_rtx (PLUS, SImode,
					 DECL_RTL (lhs),
					 gen_rtx_CONST_INT (SImode, value))));
	    return DECL_RTL (lhs);
	  }
#endif
	if (TREE_CODE (lhs) == COMPONENT_REF)
	  generate_bytecode_insns (method, TREE_OPERAND (lhs, 0), STACK_TARGET);
	else if (TREE_CODE (lhs) == ARRAY_REF)
	  {
	    generate_bytecode_insns (method,
				     TREE_OPERAND (lhs, 0), STACK_TARGET);
	    generate_bytecode_insns (method,
				     TREE_OPERAND (lhs, 1), STACK_TARGET);
	  }
	generate_bytecode_insns (method, rhs, STACK_TARGET);
	if (target != IGNORE_TARGET)
	  {
	    RESERVE (1);
	    OP1 (TYPE_IS_WIDE (type) ? OPCODE_dup2_x1 : OPCODE_dup_x1);
	  }
	if (TREE_CODE (lhs) == COMPONENT_REF)
	  {
	    tree field = TREE_OPERAND (lhs, 1);
	    field_op (field,
		      FIELD_STATIC (field) ? OPCODE_putstatic
		      : OPCODE_putfield);
	  }
	else if (TREE_CODE (lhs) == VAR_DECL
		 || TREE_CODE (lhs) == PARM_DECL)
	  {
	    if (FIELD_STATIC (lhs))
	      {
		field_op (lhs, OPCODE_putstatic);
	      }
	    else
	      {
		int index = DECL_LOCAL_INDEX (lhs);
		int opcode = adjust_typed_op (TREE_TYPE (lhs));
		if (index <= 3)
		  {
		    RESERVE (1);
		    opcode = 59 + 4 * opcode + index;
		    OP1 (opcode);  /* [ilfda]store_[0123] */
		  }
		else
		  {
		    maybe_wide (54 + opcode, index);  /* [ilfda]store */
		  }
	      }
	  }
	else if (TREE_CODE (lhs) == ARRAY_REF)
	  {
	    jopcode = OPCODE_iastore + adjust_typed_op (TREE_TYPE (lhs));
	    RESERVE(1);
	    OP1 (jopcode);
	  }
	else
	  fatal ("internal error (bad lhs to MODIFY_EXPR)");
      }
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
      generate_bytecode_insns (method, TREE_OPERAND (exp, 0), target);
      generate_bytecode_insns (method, TREE_OPERAND (exp, 1), target);
      if (target == STACK_TARGET)
	{
	  RESERVE(1);
	  OP1 (jopcode);
	}
      break;
    case CALL_EXPR:
      {
	tree t;
	for (t = TREE_OPERAND (exp, 1);  t != NULL_TREE;  t = TREE_CHAIN (t))
	  {
	    generate_bytecode_insns (method, TREE_VALUE (t), STACK_TARGET);
	  }
	t = TREE_OPERAND (exp, 0);
	if (TREE_CODE (t) == FUNCTION_DECL)
	  {
	    int index = find_methodref_index (code_cpool, t);
	    RESERVE (3);
	    if (DECL_CONSTRUCTOR_P (t))
	      OP1 (OPCODE_invokespecial);
	    else if (METHOD_STATIC (t))
	      OP1 (OPCODE_invokestatic);
	    else
	      OP1 (OPCODE_invokevirtual);
	    OP2 (index);
	    break;
	  }
      }
      /* fall through */
    default:
      error("internal error - tree code not implemented: ", TREE_CODE (exp));
    }
}

/* Generate and return a list of chunks containing the class CLAS
   in the .class file representation.  The list can be written to a
   .class file using write_chunks.  Allocate chunks from obstack WORK. */

/* Currently does not write any attributes i.e. no code. */

struct chunk *
generate_classfile (clas, work)
     tree clas;
     struct obstack *work;
{
  CPool cpool;
  struct chunk head;
  struct chunk *chunk;
  struct chunk *cpool_chunk;
  char *ptr;
  int i;
  char *fields_count_ptr;
  int fields_count = 0;
  char *methods_count_ptr;
  int methods_count = 0;
  tree part;
  int total_supers
    = clas == object_type_node ? 0
    : TREE_VEC_LENGTH (TYPE_BINFO_BASETYPES (clas));

  chunk = alloc_chunk (&head, NULL, 8, work);
  ptr = chunk->data;
  PUT4 (0xCafeBabe);  /* Magic number */
  PUT2 (3);  /* Minor version */
  PUT2 (45);  /* Major version */
  
  CPOOL_INIT(&cpool);
  cpool_chunk = chunk = alloc_chunk (chunk, NULL, 0, work);

  /* Next allocate the chunk containing acces_flags through fields_counr. */
  if (clas == object_type_node)
    i = 10;
  else
    i = 8 + 2 * total_supers;
  chunk = alloc_chunk (chunk, NULL, i, work);
  ptr = chunk->data;
  i = get_access_flags (TYPE_NAME (clas));  PUT2 (i); /* acces_flags */
  i = find_class_constant (&cpool, clas);  PUT2 (i);  /* this_class */
  if (clas == object_type_node)
    {
      PUT2(0);  /* super_class */
      PUT2(0);  /* interfaces_count */
    }
  else
    {
      tree basetypes = TYPE_BINFO_BASETYPES (clas);
      tree base = BINFO_TYPE (TREE_VEC_ELT (basetypes, 0));
      int j = find_class_constant (&cpool, base);  PUT2 (j);  /* super_class */
      PUT2 (total_supers - 1);  /* interfaces_count */
      for (i = 1;  i < total_supers;  i++)
	{
	  base = BINFO_TYPE (TREE_VEC_ELT (basetypes, i));
	  j = find_class_constant (&cpool, base);
	  PUT2 (j);
	}
    }
  fields_count_ptr = ptr;

  for (part = TYPE_FIELDS (clas);  part;  part = TREE_CHAIN (part))
    {
      if (DECL_NAME (part) == NULL_TREE)
	continue;
      chunk = alloc_chunk (chunk, NULL, 8, work);
      ptr = chunk->data;
      i = get_access_flags (part);  PUT2 (i);
      i = find_utf8_constant (&cpool, DECL_NAME (part));  PUT2 (i);
      i = find_utf8_constant (&cpool, build_java_signature (TREE_TYPE (part)));
      PUT2(i);
      PUT2 (0);  /* attributes_count */
      /* FIXME - emit ConstantValue attribute when appropriate */
      fields_count++;
    }
  ptr = fields_count_ptr;  PUT2 (fields_count);

  chunk = alloc_chunk (chunk, NULL, 2, work);
  ptr = methods_count_ptr = chunk->data;
  PUT2 (0);

  for (part = TYPE_METHODS (clas);  part;  part = TREE_CHAIN (part))
    {
      tree body = BLOCK_EXPR_BODY (DECL_FUNCTION_BODY (part));
      int linenumber_size;  /* 4 * number of line number entries */
      chunk = alloc_chunk (chunk, NULL, 8, work);
      ptr = chunk->data;
      i = get_access_flags (part);  PUT2 (i);
      i = find_utf8_constant (&cpool, DECL_NAME (part));  PUT2 (i);
      i = find_utf8_constant (&cpool, build_java_signature (TREE_TYPE (part)));
      PUT2 (i);
      PUT2 (body != NULL_TREE ? 1 : 0);   /* attributes_count */
      if (body != NULL_TREE)
	{
	  int code_attributes_count = 0;
	  int linenumber_size;  /* 4 * number of line number entries */
	  int localvartable_size;  /* 10 * number of local variable entries */
	  static tree Code_node = NULL_TREE;
	  tree t;
	  char *attr_len_ptr;
	  int code_length;
	  if (Code_node == NULL_TREE)
	    Code_node = get_identifier ("Code");
	  chunk = alloc_chunk (chunk, NULL, 14, work);
	  ptr = chunk->data;
	  i = find_utf8_constant (&cpool, Code_node);  PUT2 (i);
	  attr_len_ptr = ptr;
	  BUFFER_RESET (&bytecode);
	  BUFFER_RESET (&localvartable);
	  BUFFER_RESET (&linenumbers);
	  BUFFER_RESET (&localvars);
	  code_SP = 0;
	  code_SP_max = 0;
	  code_cpool = &cpool;
	  for (t = DECL_ARGUMENTS (part);  t != NULL_TREE;  t = TREE_CHAIN (t))
	    localvar_alloc (t, 0);
	  generate_bytecode_insns (part, body, IGNORE_TARGET);
	  code_length = PC;	
	  for (t = DECL_ARGUMENTS (part);  t != NULL_TREE;  t = TREE_CHAIN (t))
	    localvar_free (t, code_length);
	  linenumber_size = BUFFER_LENGTH (&linenumbers);
	  localvartable_size = BUFFER_LENGTH (&localvartable);
	  chunk = alloc_chunk (chunk, NULL, code_length, work);
	  bcopy (bytecode.data, chunk->data, code_length);
	  ptr = attr_len_ptr;
	  i = 8 + code_length + 4;
	  if (linenumber_size > 0)
	    {
	      code_attributes_count++;
	      i += 8 + linenumber_size;
	    }
	  if (localvartable_size > 0)
	    {
	      code_attributes_count++;
	      i += 8 + localvartable_size;
	    }
	  PUT4 (i); /* attribute_length */
	  PUT2 (code_SP_max);  /* max_stack */
	  PUT2 (localvar_max);  /* max_locals */
	  PUT4 (code_length);
	  chunk = alloc_chunk (chunk, NULL, 4, work);
	  ptr = chunk->data;
	  PUT2 (0);  /* exception_table_length */
	  PUT2 (code_attributes_count);

	  /* Write the LineNumberTable attribute. */
	  if (linenumber_size > 0)
	    {
	      static tree LineNumberTable_node = NULL_TREE;
	      chunk = alloc_chunk (chunk, NULL, 8 + linenumber_size, work);
	      ptr = chunk->data;
	      if (LineNumberTable_node == NULL_TREE)
		LineNumberTable_node = get_identifier ("LineNumberTable");
	      i = find_utf8_constant (&cpool, LineNumberTable_node);
	      PUT2 (i);  /* attribute_name_index */
	      i = 2 + linenumber_size;  PUT4 (i);  /* attribute_length */
	      i = linenumber_size >> 2;  PUT2 (i);
	      PUTN (linenumbers.data, linenumber_size);
	    }

	  /* Write the LocalVariableTable attribute. */
	  if (localvartable_size > 0)
	    {
	      static tree LocalVariableTable_node = NULL_TREE;
	      chunk = alloc_chunk (chunk, NULL, 8 + localvartable_size, work);
	      ptr = chunk->data;
	      if (LocalVariableTable_node == NULL_TREE)
		LocalVariableTable_node = get_identifier("LocalVariableTable");
	      i = find_utf8_constant (&cpool, LocalVariableTable_node);
	      PUT2 (i);  /* attribute_name_index */
	      i = 2 + localvartable_size;  PUT4 (i);  /* attribute_length */
	      i = localvartable_size / 10;  PUT2 (i);
	      PUTN (localvartable.data, localvartable_size);
	    }
	}
      methods_count++;
    }
  ptr = methods_count_ptr;  PUT2 (methods_count);

  chunk = alloc_chunk (chunk, NULL, 2, work);
  ptr = chunk->data;
  PUT2 (0);  /* attributes_count */

  /* New finally generate the contents of the constant pool chunk. */
  i = count_constant_pool_bytes (&cpool);
  ptr = obstack_alloc (work, i);
  cpool_chunk->data = ptr;
  cpool_chunk->size = i;
  write_constant_pool (&cpool, ptr, i);
  CPOOL_FINISH (&cpool);
  return head.next;
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
  char *class_file_name = make_class_file_name (clas);
  struct chunk *chunks;
  FILE* stream = fopen (class_file_name, "wb");
  if (stream == NULL)
    fatal ("failed to open `%s' for writing", class_file_name);
  chunks = generate_classfile (clas, work);
  write_chunks (stream, chunks);
  if (fclose (stream))
    fatal ("failed to close after writing `%s'", class_file_name);
  obstack_free (work, chunks);
}
