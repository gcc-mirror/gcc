/* This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>. */

#include "rust.h"

static bool initilized = false;

static const char * opcodeStrings [] = {
  /* [D_IDENTIFIER] */         "identifier",
  /* [D_T_INTEGER] */          "literal_integer",
  /* [D_T_FLOAT] */            "literal_float",
  /* [D_T_STRING] */           "literal_string",
  /* [D_T_LIST] */             "liteal_list",
  /* [D_VAR_DECL] */           "var_decl",
  /* [D_MODIFY_EXPR] */        "modify_expr",
  /* [D_MULT_EXPR] */          "multiply_expr",
  /* [D_DIVD_EXPR] */          "divide_expr",
  /* [D_ADD_EXPR] */           "plus_expr",
  /* [D_MINUS_EXPR] */         "minus_expr",
  /* [D_EQ_EQ_EXPR] */         "equivilant_expr",
  /* [D_LESS_EXPR] */          "less_than_expr",
  /* [D_LESS_EQ_EXPR] */       "less_eq_expr",
  /* [D_GREATER_EXPR] */       "greater_expr",
  /* [D_GREATER_EQ_EXPR] */    "greater_eq_expr",
  /* [D_NOT_EQ_EXPR] */        "not_equal_expr",
  /* [D_CALL_EXPR] */          "call_expr",
  /* [D_ATTRIB_EXPR] */        "attribute_reference",
  /* [D_ACC_EXPR] */           "accessor_reference",
  /* [D_STRUCT_METHOD] */      "struct_method",
  /* [D_STRUCT_WHILE] */       "struct_while",
  /* [D_STRUCT_LOOP] */        "struct_loop",
  /* [D_D_EXPR] */             "enc_expression",
  /* [D_TD_COM] */             "TD_COM",
  /* [D_TD_DOT] */             "TD_DOT",
  /* [D_TD_NULL] */            "TD_NULL",
  /* [D_PRIMITIVE] */          "primitive",
  /* [D_STRUCT_IF] */          "struct_if",
  /* [D_STRUCT_ELIF] */        "struct_elif",
  /* [D_STRUCT_ELSE] */        "struct_else",
  /* [D_STRUCT_CONDITIONAL] */ "struct_conditional",
  /* [RTYPE_BOOL] */           "type_bool",
  /* [RTYPE_INT] */            "type_int",
  /* [RTYPE_FLOAT] */          "type_float",
  /* [RTYPE_UINT] */           "type_uint",
  /* [RTYPE_INFER] */          "type_infer",
  /* [D_PARAMETER] */          "parameter",
  /* [D_STRUCT_TYPE] */        "struct_definition",
  /* [D_STRUCT_PARAM] */       "struct_init_param",
  /* [D_STRUCT_INIT] */        "struct_initilization",
  /* [RTYPE_USER_STRUCT] */    "user_struct_type",
  /* [D_STRUCT_ENUM] */        "struct_enum",
  /* [D_STRUCT_IMPL] */        "impl_block",
  /* [D_BOOLEAN] */            "d_boolean",
  /* [D_T_BOOL] */             "d_t_bool",
  /* [C_BREAK_STMT] */         "break_stmt",
  /* [C_CONT_STMT] */          "continue_stmt",
  /* [C_RETURN_STMT] */        "return_stmt",
};

const char *
rdot_getOpString_T (const opcode_t o)
{
  return opcodeStrings [o];
}

const char *
rdot_getOpString (const rdot dot)
{
  return rdot_getOpString_T (RDOT_TYPE (dot));
}

void rdot_init (void)
{
  if (initilized)
    return;
  //... probably should get rid of this function ...
  initilized = true;
}

rdot rdot_build_varDecl (rdot type, bool final, rdot id)
{
  rdot decl = rdot_build_decl2 (D_VAR_DECL, id, type);
  RDOT_qual (decl) = final;
  return decl;
}

rdot rdot_alloc (void)
{
  rdot retval = (struct grs_tree_dot *)
    xmalloc (sizeof (struct grs_tree_dot));
  gcc_assert (retval);
  memset (retval, 0, sizeof (struct grs_tree_dot));
  RDOT_LOCATION (retval) = UNKNOWN_LOCATION;
  return retval;
}

rdot rdot_build_decl1 (opcode_t o, rdot t1)
{
  rdot decl = RDOT_alloc;

  RDOT_TYPE(decl) = o;
  RDOT_T_FIELD(decl) = D_TD_NULL;
  RDOT_FIELD(decl) = NULL_DOT;

  decl->opaT = D_TD_DOT;
  decl->opa.t = t1;
  decl->opbT = D_TD_NULL;

  RDOT_CHAIN(decl) = NULL_DOT;

  return decl;
}

rdot rdot_build_decl2 (opcode_t o, rdot t1, rdot t2)
{
  rdot decl = RDOT_alloc;

  RDOT_TYPE (decl) = o;
  if ((o == D_VAR_DECL)
      || (o == D_MODIFY_EXPR)
      || (o == D_ADD_EXPR)
      || (o == D_MINUS_EXPR)
      || (o == D_MULT_EXPR)
      || (o == D_DIVD_EXPR)
      || (o == D_CALL_EXPR)
      || (o == D_EQ_EQ_EXPR)
      || (o == D_LESS_EXPR)
      || (o == D_LESS_EQ_EXPR)
      || (o == D_GREATER_EXPR)
      || (o == D_GREATER_EQ_EXPR)
      || (o == D_NOT_EQ_EXPR)
      || (o == D_ATTRIB_REF)
      || (o == D_ACC_EXPR)
      || (o == D_STRUCT_INIT)
      )
    RDOT_T_FIELD(decl) = D_D_EXPR;
  else
    RDOT_T_FIELD(decl) = D_TD_NULL;

  RDOT_FIELD (decl) = NULL_DOT;

  decl->opaT = D_TD_DOT;
  decl->opa.t = t1;
  decl->opbT = D_TD_DOT;
  decl->opb.t = t2;

  RDOT_CHAIN(decl) = NULL_DOT;

  return decl;
}

rdot rdot_build_fndecl (rdot ident, bool pub, rdot params, rdot rtype, rdot suite)
{
  rdot decl = RDOT_alloc;

  RDOT_TYPE (decl) = D_STRUCT_METHOD;
  RDOT_T_FIELD (decl) = D_TD_NULL;

  RDOT_FIELD (decl) = ident;
  RDOT_FIELD2 (decl) = rtype;
  DOT_RETVAL (decl) = pub;

  decl->opaT = D_TD_DOT;
  decl->opa.t = params;
  decl->opbT = D_TD_DOT;
  decl->opb.t = suite;

  RDOT_CHAIN(decl) = NULL_DOT;

  return decl;
}

rdot rdot_build_integer (const int i)
{
  rdot decl = RDOT_alloc;
  RDOT_TYPE(decl) = D_PRIMITIVE;

  RDOT_FIELD(decl) = NULL_DOT;
  RDOT_T_FIELD(decl) = D_D_EXPR;

  decl->opaT = D_TD_COM;
  decl->opa.tc.T = D_T_INTEGER;
  decl->opa.tc.o.integer = i;

  return decl;
}

rdot rdot_build_float (const float f)
{
  rdot decl = RDOT_alloc;
  RDOT_TYPE(decl) = D_PRIMITIVE;

  RDOT_FIELD(decl) = NULL_DOT;
  RDOT_T_FIELD(decl) = D_D_EXPR;

  decl->opaT = D_TD_COM;
  decl->opa.tc.T = D_T_FLOAT;
  decl->opa.tc.o.ffloat = f;

  return decl;
}

rdot rdot_build_string (const char * s)
{
  rdot decl = RDOT_alloc;
  RDOT_TYPE (decl) = D_PRIMITIVE;

  RDOT_FIELD (decl) = NULL_DOT;
  RDOT_T_FIELD (decl) = D_D_EXPR;

  decl->opaT = D_TD_COM;
  decl->opa.tc.T = D_T_STRING;
  decl->opa.tc.o.string = xstrdup (s);

  return decl;
}

rdot rdot_build_identifier (const char * s)
{
  rdot decl = RDOT_alloc;

  RDOT_TYPE(decl) = D_IDENTIFIER;
  RDOT_FIELD(decl) = NULL_DOT;
  RDOT_T_FIELD(decl) = D_D_EXPR;

  decl->opaT = D_TD_COM;
  decl->opa.tc.T = D_T_STRING;
  decl->opa.tc.o.string = xstrdup (s);

  decl->opbT = D_TD_NULL;

  RDOT_CHAIN(decl) = NULL_DOT;

  return decl;
}

rdot rdot_build_bool (bool val)
{
  rdot decl = RDOT_alloc;

  RDOT_TYPE(decl) = D_BOOLEAN;
  RDOT_FIELD(decl) = NULL_DOT;
  RDOT_T_FIELD(decl) = D_D_EXPR;

  decl->opaT = D_TD_COM;
  decl->opa.tc.T = D_T_BOOL;
  decl->opa.tc.o.boolean = val;

  decl->opbT = D_TD_NULL;

  RDOT_CHAIN(decl) = NULL_DOT;

  return decl;
}
