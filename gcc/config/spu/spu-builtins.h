/* Copyright (C) 2006 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2 of the License, or (at your option) 
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

/* built-ins */

enum spu_builtin_type_index
{
  SPU_BTI_END_OF_PARAMS,

  /* We create new type nodes for these. */
  SPU_BTI_V16QI,
  SPU_BTI_V8HI,
  SPU_BTI_V4SI,
  SPU_BTI_V2DI,
  SPU_BTI_V4SF,
  SPU_BTI_V2DF,
  SPU_BTI_UV16QI,
  SPU_BTI_UV8HI,
  SPU_BTI_UV4SI,
  SPU_BTI_UV2DI,

  /* A 16-byte type. (Implemented with V16QI_type_node) */
  SPU_BTI_QUADWORD,

  /* These all correspond to intSI_type_node */
  SPU_BTI_7,
  SPU_BTI_S7,
  SPU_BTI_U7,
  SPU_BTI_S10,
  SPU_BTI_S10_4,
  SPU_BTI_U14,
  SPU_BTI_16,
  SPU_BTI_S16,
  SPU_BTI_S16_2,
  SPU_BTI_U16,
  SPU_BTI_U16_2,
  SPU_BTI_U18,

  /* These correspond to the standard types */
  SPU_BTI_INTQI, 
  SPU_BTI_INTHI, 
  SPU_BTI_INTSI, 
  SPU_BTI_INTDI, 

  SPU_BTI_UINTQI,
  SPU_BTI_UINTHI,
  SPU_BTI_UINTSI,
  SPU_BTI_UINTDI,

  SPU_BTI_FLOAT, 
  SPU_BTI_DOUBLE,

  SPU_BTI_VOID,   
  SPU_BTI_PTR,   

  SPU_BTI_MAX
};

#define V16QI_type_node               (spu_builtin_types[SPU_BTI_V16QI])
#define V8HI_type_node                (spu_builtin_types[SPU_BTI_V8HI])
#define V4SI_type_node                (spu_builtin_types[SPU_BTI_V4SI])
#define V2DI_type_node                (spu_builtin_types[SPU_BTI_V2DI])
#define V4SF_type_node                (spu_builtin_types[SPU_BTI_V4SF])
#define V2DF_type_node                (spu_builtin_types[SPU_BTI_V2DF])
#define unsigned_V16QI_type_node      (spu_builtin_types[SPU_BTI_UV16QI])
#define unsigned_V8HI_type_node       (spu_builtin_types[SPU_BTI_UV8HI])
#define unsigned_V4SI_type_node       (spu_builtin_types[SPU_BTI_UV4SI])
#define unsigned_V2DI_type_node       (spu_builtin_types[SPU_BTI_UV2DI])

extern GTY(()) tree spu_builtin_types[SPU_BTI_MAX];

/* Some builtins require special handling.  This enum differentiates. */
enum spu_builtin_type {
    B_INSN,
    B_JUMP,
    B_BISLED,
    B_CALL,
    B_HINT,
    B_OVERLOAD, 
    B_INTERNAL
};

typedef enum {
#define DEF_BUILTIN(fcode, icode, name, type, params) fcode,
#include "spu-builtins.def"
#undef DEF_BUILTIN
   NUM_SPU_BUILTINS
} spu_function_code;

struct spu_builtin_description {
    spu_function_code fcode;
    enum insn_code icode;
    const char *name;
    enum spu_builtin_type type;

    /* The first element of parm is always the return type.  The rest
     * are a zero terminated list of parameters. */
    int parm[5];

    tree fndecl;
};

extern GTY(()) struct spu_builtin_description spu_builtins[];



