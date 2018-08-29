#include <libgccjit.h>
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void * user_data)
{
  gcc_jit_type *type_void = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *type_int = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_type *type_unsigned_char = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UNSIGNED_CHAR);
  gcc_jit_type *type_void_ptr =
    gcc_jit_type_get_pointer (type_void);
  gcc_jit_type *type_void_ptr_ptr =
    gcc_jit_type_get_pointer (type_void_ptr);
  gcc_jit_type *type_unsigned_char__ =
    gcc_jit_type_get_pointer (type_unsigned_char);
  gcc_jit_field *field_x =
    gcc_jit_context_new_field (ctxt,
                               NULL, /* gcc_jit_location *loc */
                               type_int, /* gcc_jit_type *type, */
                               "x"); /* const char *name */
  gcc_jit_field *field_y =
    gcc_jit_context_new_field (ctxt,
                               NULL, /* gcc_jit_location *loc */
                               type_int, /* gcc_jit_type *type, */
                               "y"); /* const char *name */
  gcc_jit_struct *struct_struct_ip_coord =
    gcc_jit_context_new_opaque_struct (ctxt,
                                       NULL, /* gcc_jit_location *loc */
                                       "ip_coord"); /* const char *name */
  gcc_jit_field *fields_fields_0x18dc9d0[2] = {
    field_x,
    field_y,
  };
  gcc_jit_struct_set_fields (struct_struct_ip_coord, /* gcc_jit_struct *struct_type */
                             NULL, /* gcc_jit_location *loc */
                             2, /* int num_fields */
                             fields_fields_0x18dc9d0); /* gcc_jit_field **fields */
  gcc_jit_field *field_size =
    gcc_jit_context_new_field (ctxt,
                               NULL, /* gcc_jit_location *loc */
                               gcc_jit_struct_as_type (struct_struct_ip_coord), /* gcc_jit_type *type, */
                               "size"); /* const char *name */
  gcc_jit_field *field_imrow =
    gcc_jit_context_new_field (ctxt,
                               NULL, /* gcc_jit_location *loc */
                               type_void_ptr_ptr, /* gcc_jit_type *type, */
                               "imrow"); /* const char *name */
  gcc_jit_struct *struct_struct_ip_image =
    gcc_jit_context_new_opaque_struct (ctxt,
                                       NULL, /* gcc_jit_location *loc */
                                       "ip_image"); /* const char *name */
  gcc_jit_field *fields_fields_0x18dd310[] = {
    field_size,
    field_imrow
  };
  gcc_jit_struct_set_fields (struct_struct_ip_image, /* gcc_jit_struct *struct_type */
                             NULL, /* gcc_jit_location *loc */
                             2, /* int num_fields */
                             fields_fields_0x18dd310); /* gcc_jit_field **fields */
  gcc_jit_type *type_struct_ip_image__ =
    gcc_jit_type_get_pointer (gcc_jit_struct_as_type (struct_struct_ip_image));
  gcc_jit_param *param_dest =
    gcc_jit_context_new_param (ctxt,
                               NULL, /* gcc_jit_location *loc */
                               type_struct_ip_image__, /*gcc_jit_type *type */
                               "dest"); /* const char *name */
  gcc_jit_param *param_src =
    gcc_jit_context_new_param (ctxt,
                               NULL, /* gcc_jit_location *loc */
                               type_struct_ip_image__, /*gcc_jit_type *type */
                               "src"); /* const char *name */
  gcc_jit_param *params_for_func_ip_jit_im_add_clip_UBYTE[2] = {
    param_dest,
    param_src,
  };
  gcc_jit_function *func_ip_jit_im_add_clip_UBYTE =
    gcc_jit_context_new_function (ctxt, /* gcc_jit_context *ctxt */
                                  NULL, /* gcc_jit_location *loc */
                                  GCC_JIT_FUNCTION_EXPORTED, /* enum gcc_jit_function_kind kind */
                                  type_void, /* gcc_jit_type *return_type */
                                  "ip_jit_im_add_clip_UBYTE", /* const char *name */
                                  2, /* int num_params */
                                  params_for_func_ip_jit_im_add_clip_UBYTE, /* gcc_jit_param **params */
                                  0); /* int is_variadic */
  gcc_jit_lvalue *local_rowlen =
    gcc_jit_function_new_local (func_ip_jit_im_add_clip_UBYTE, /* gcc_jit_function *func */
                                NULL, /* gcc_jit_location *loc */
                                type_int, /* gcc_jit_type *type */
                                "rowlen"); /* const char *name */
  gcc_jit_lvalue *local_numrows =
    gcc_jit_function_new_local (func_ip_jit_im_add_clip_UBYTE, /* gcc_jit_function *func */
                                NULL, /* gcc_jit_location *loc */
                                type_int, /* gcc_jit_type *type */
                                "numrows"); /* const char *name */
  gcc_jit_lvalue *local_j =
    gcc_jit_function_new_local (func_ip_jit_im_add_clip_UBYTE, /* gcc_jit_function *func */
                                NULL, /* gcc_jit_location *loc */
                                type_int, /* gcc_jit_type *type */
                                "j"); /* const char *name */
  gcc_jit_lvalue *local_dptr =
    gcc_jit_function_new_local (func_ip_jit_im_add_clip_UBYTE, /* gcc_jit_function *func */
                                NULL, /* gcc_jit_location *loc */
                                type_unsigned_char__, /* gcc_jit_type *type */
                                "dptr"); /* const char *name */
  gcc_jit_lvalue *local_sptr =
    gcc_jit_function_new_local (func_ip_jit_im_add_clip_UBYTE, /* gcc_jit_function *func */
                                NULL, /* gcc_jit_location *loc */
                                type_unsigned_char__, /* gcc_jit_type *type */
                                "sptr"); /* const char *name */
  gcc_jit_lvalue *local_i =
    gcc_jit_function_new_local (func_ip_jit_im_add_clip_UBYTE, /* gcc_jit_function *func */
                                NULL, /* gcc_jit_location *loc */
                                type_int, /* gcc_jit_type *type */
                                "i"); /* const char *name */
  gcc_jit_block *block_F1 =
    gcc_jit_function_new_block (func_ip_jit_im_add_clip_UBYTE, "F1");
  gcc_jit_block *block_C1 =
    gcc_jit_function_new_block (func_ip_jit_im_add_clip_UBYTE, "C1");
  gcc_jit_block *block_L1 =
    gcc_jit_function_new_block (func_ip_jit_im_add_clip_UBYTE, "L1");
  gcc_jit_block *block_C2 =
    gcc_jit_function_new_block (func_ip_jit_im_add_clip_UBYTE, "C2");
  gcc_jit_block *block_L2 =
    gcc_jit_function_new_block (func_ip_jit_im_add_clip_UBYTE, "L2");
  gcc_jit_block *block_A2 =
    gcc_jit_function_new_block (func_ip_jit_im_add_clip_UBYTE, "A2");
  gcc_jit_block *block_A1 =
    gcc_jit_function_new_block (func_ip_jit_im_add_clip_UBYTE, "A1");
  gcc_jit_lvalue *lvalue_dest__size=
    gcc_jit_rvalue_dereference_field (gcc_jit_param_as_rvalue (param_dest), /* gcc_jit_rvalue *ptr */
                                      NULL, /* gcc_jit_location *loc */
                                      field_size); /* gcc_jit_field *field */
  gcc_jit_rvalue *rvalue_dest__size_x = 
    gcc_jit_rvalue_access_field (gcc_jit_lvalue_as_rvalue (lvalue_dest__size), /*gcc_jit_rvalue *struct_or_union */
                                 NULL, /*gcc_jit_location *loc */
                                 field_x);
  gcc_jit_block_add_assignment (block_F1, /*gcc_jit_block *block */
                                NULL, /* gcc_jit_location *loc */
                                local_rowlen, /* gcc_jit_lvalue *lvalue */
                                rvalue_dest__size_x); /* gcc_jit_rvalue *rvalue */
  gcc_jit_rvalue *rvalue_dest__size_y = 
    gcc_jit_rvalue_access_field (gcc_jit_lvalue_as_rvalue (lvalue_dest__size), /*gcc_jit_rvalue *struct_or_union */
                                 NULL, /*gcc_jit_location *loc */
                                 field_y);
  gcc_jit_block_add_assignment (block_F1, /*gcc_jit_block *block */
                                NULL, /* gcc_jit_location *loc */
                                local_numrows, /* gcc_jit_lvalue *lvalue */
                                rvalue_dest__size_y); /* gcc_jit_rvalue *rvalue */
  gcc_jit_rvalue *rvalue__int_0 =
    gcc_jit_context_new_rvalue_from_int (ctxt, /* gcc_jit_context *ctxt */
                                         type_int, /* gcc_jit_type *numeric_type */
                                         0); /* int value */
  gcc_jit_block_add_assignment (block_F1, /*gcc_jit_block *block */
                                NULL, /* gcc_jit_location *loc */
                                local_j, /* gcc_jit_lvalue *lvalue */
                                rvalue__int_0); /* gcc_jit_rvalue *rvalue */
  gcc_jit_block_end_with_jump (block_F1, /*gcc_jit_block *block */
                               NULL, /* gcc_jit_location *loc */
                               block_C1); /* gcc_jit_block *target */
  gcc_jit_rvalue *rvalue_j___numrows =
    gcc_jit_context_new_comparison (ctxt,
                                    NULL, /* gcc_jit_location *loc */
                                    GCC_JIT_COMPARISON_LT, /* enum gcc_jit_comparison op */
                                    gcc_jit_lvalue_as_rvalue (local_j), /* gcc_jit_rvalue *a */
                                    gcc_jit_lvalue_as_rvalue (local_numrows)); /* gcc_jit_rvalue *b */
  gcc_jit_block_end_with_conditional (block_C1, /*gcc_jit_block *block */
                                      NULL, /* gcc_jit_location *loc */
                                      rvalue_j___numrows, /* gcc_jit_rvalue *boolval */
                                      block_L1, /* gcc_jit_block *on_true */
                                      block_A1); /* gcc_jit_block *on_false */
  gcc_jit_lvalue *lvalue_dest__imrow=
    gcc_jit_rvalue_dereference_field (gcc_jit_param_as_rvalue (param_dest), /* gcc_jit_rvalue *ptr */
                                      NULL, /* gcc_jit_location *loc */
                                      field_imrow); /* gcc_jit_field *field */
  gcc_jit_lvalue *lvalue_dest__imrow_j_ = 
    gcc_jit_context_new_array_access (ctxt, /* gcc_jit_context *ctxt */
                                      NULL, /*gcc_jit_location *loc */
                                      gcc_jit_lvalue_as_rvalue (lvalue_dest__imrow), /* gcc_jit_rvalue *ptr */
                                      gcc_jit_lvalue_as_rvalue (local_j)); /* gcc_jit_rvalue *index */
  gcc_jit_rvalue *rvalue__unsigned_char___dest__imrow_j_ =
    gcc_jit_context_new_cast (ctxt,
                              NULL, /* gcc_jit_location *loc */
                              gcc_jit_lvalue_as_rvalue (lvalue_dest__imrow_j_), /* gcc_jit_rvalue *rvalue */
                              type_unsigned_char__); /* gcc_jit_type *type */
  gcc_jit_block_add_assignment (block_L1, /*gcc_jit_block *block */
                                NULL, /* gcc_jit_location *loc */
                                local_dptr, /* gcc_jit_lvalue *lvalue */
                                rvalue__unsigned_char___dest__imrow_j_); /* gcc_jit_rvalue *rvalue */
  gcc_jit_lvalue *lvalue_src__imrow=
    gcc_jit_rvalue_dereference_field (gcc_jit_param_as_rvalue (param_src), /* gcc_jit_rvalue *ptr */
                                      NULL, /* gcc_jit_location *loc */
                                      field_imrow); /* gcc_jit_field *field */
  gcc_jit_lvalue *lvalue_src__imrow_j_ = 
    gcc_jit_context_new_array_access (ctxt, /* gcc_jit_context *ctxt */
                                      NULL, /*gcc_jit_location *loc */
                                      gcc_jit_lvalue_as_rvalue (lvalue_src__imrow), /* gcc_jit_rvalue *ptr */
                                      gcc_jit_lvalue_as_rvalue (local_j)); /* gcc_jit_rvalue *index */
  gcc_jit_rvalue *rvalue__unsigned_char___src__imrow_j_ =
    gcc_jit_context_new_cast (ctxt,
                              NULL, /* gcc_jit_location *loc */
                              gcc_jit_lvalue_as_rvalue (lvalue_src__imrow_j_), /* gcc_jit_rvalue *rvalue */
                              type_unsigned_char__); /* gcc_jit_type *type */
  gcc_jit_block_add_assignment (block_L1, /*gcc_jit_block *block */
                                NULL, /* gcc_jit_location *loc */
                                local_sptr, /* gcc_jit_lvalue *lvalue */
                                rvalue__unsigned_char___src__imrow_j_); /* gcc_jit_rvalue *rvalue */
  gcc_jit_rvalue *rvalue__int_0_0x18dd890 =
    gcc_jit_context_new_rvalue_from_int (ctxt, /* gcc_jit_context *ctxt */
                                         type_int, /* gcc_jit_type *numeric_type */
                                         0); /* int value */
  gcc_jit_block_add_assignment (block_L1, /*gcc_jit_block *block */
                                NULL, /* gcc_jit_location *loc */
                                local_i, /* gcc_jit_lvalue *lvalue */
                                rvalue__int_0_0x18dd890); /* gcc_jit_rvalue *rvalue */
  gcc_jit_block_end_with_jump (block_L1, /*gcc_jit_block *block */
                               NULL, /* gcc_jit_location *loc */
                               block_C2); /* gcc_jit_block *target */
  gcc_jit_rvalue *rvalue_i___rowlen =
    gcc_jit_context_new_comparison (ctxt,
                                    NULL, /* gcc_jit_location *loc */
                                    GCC_JIT_COMPARISON_LT, /* enum gcc_jit_comparison op */
                                    gcc_jit_lvalue_as_rvalue (local_i), /* gcc_jit_rvalue *a */
                                    gcc_jit_lvalue_as_rvalue (local_rowlen)); /* gcc_jit_rvalue *b */
  gcc_jit_block_end_with_conditional (block_C2, /*gcc_jit_block *block */
                                      NULL, /* gcc_jit_location *loc */
                                      rvalue_i___rowlen, /* gcc_jit_rvalue *boolval */
                                      block_L2, /* gcc_jit_block *on_true */
                                      block_A2); /* gcc_jit_block *on_false */
  gcc_jit_lvalue *dereference__dptr =
    gcc_jit_rvalue_dereference (gcc_jit_lvalue_as_rvalue (local_dptr), /* gcc_jit_rvalue *rvalue */
                                NULL); /* gcc_jit_location *loc */
  gcc_jit_lvalue *dereference__sptr =
    gcc_jit_rvalue_dereference (gcc_jit_lvalue_as_rvalue (local_sptr), /* gcc_jit_rvalue *rvalue */
                                NULL); /* gcc_jit_location *loc */
  gcc_jit_block *block_p_C1_true =
    gcc_jit_function_new_block (func_ip_jit_im_add_clip_UBYTE, "p_C1_true");
  gcc_jit_block *block_p_C1_end =
    gcc_jit_function_new_block (func_ip_jit_im_add_clip_UBYTE, "p_C1_end");
  gcc_jit_lvalue *local_ival =
    gcc_jit_function_new_local (func_ip_jit_im_add_clip_UBYTE, /* gcc_jit_function *func */
                                NULL, /* gcc_jit_location *loc */
                                type_int, /* gcc_jit_type *type */
                                "ival"); /* const char *name */
  gcc_jit_rvalue *rvalue__int__dptr =
    gcc_jit_context_new_cast (ctxt,
                              NULL, /* gcc_jit_location *loc */
                              gcc_jit_lvalue_as_rvalue (dereference__dptr), /* gcc_jit_rvalue *rvalue */
                              type_int); /* gcc_jit_type *type */
  gcc_jit_rvalue *rvalue__int__sptr =
    gcc_jit_context_new_cast (ctxt,
                              NULL, /* gcc_jit_location *loc */
                              gcc_jit_lvalue_as_rvalue (dereference__sptr), /* gcc_jit_rvalue *rvalue */
                              type_int); /* gcc_jit_type *type */
  gcc_jit_rvalue *rvalue__int__dptr____int__sptr =
    gcc_jit_context_new_binary_op (ctxt,
                                   NULL, /* gcc_jit_location *loc */
                                   GCC_JIT_BINARY_OP_PLUS, /* enum gcc_jit_binary_op op */
                                   type_int, /* gcc_jit_type *result_type */
                                   rvalue__int__dptr, /* gcc_jit_rvalue *a */
                                   rvalue__int__sptr); /* gcc_jit_rvalue *b */
  gcc_jit_block_add_assignment (block_L2, /*gcc_jit_block *block */
                                NULL, /* gcc_jit_location *loc */
                                local_ival, /* gcc_jit_lvalue *lvalue */
                                rvalue__int__dptr____int__sptr); /* gcc_jit_rvalue *rvalue */
  gcc_jit_rvalue *rvalue__int_255 =
    gcc_jit_context_new_rvalue_from_int (ctxt, /* gcc_jit_context *ctxt */
                                         type_int, /* gcc_jit_type *numeric_type */
                                         255); /* int value */
  gcc_jit_rvalue *rvalue_ival____int_255 =
    gcc_jit_context_new_comparison (ctxt,
                                    NULL, /* gcc_jit_location *loc */
                                    GCC_JIT_COMPARISON_GT, /* enum gcc_jit_comparison op */
                                    gcc_jit_lvalue_as_rvalue (local_ival), /* gcc_jit_rvalue *a */
                                    rvalue__int_255); /* gcc_jit_rvalue *b */
  gcc_jit_block_end_with_conditional (block_L2, /*gcc_jit_block *block */
                                      NULL, /* gcc_jit_location *loc */
                                      rvalue_ival____int_255, /* gcc_jit_rvalue *boolval */
                                      block_p_C1_true, /* gcc_jit_block *on_true */
                                      block_p_C1_end); /* gcc_jit_block *on_false */
  gcc_jit_block_add_assignment (block_p_C1_true, /*gcc_jit_block *block */
                                NULL, /* gcc_jit_location *loc */
                                local_ival, /* gcc_jit_lvalue *lvalue */
                                rvalue__int_255); /* gcc_jit_rvalue *rvalue */
  gcc_jit_block_end_with_jump (block_p_C1_true, /*gcc_jit_block *block */
                               NULL, /* gcc_jit_location *loc */
                               block_p_C1_end); /* gcc_jit_block *target */
  gcc_jit_rvalue *rvalue__unsigned_char_ival =
    gcc_jit_context_new_cast (ctxt,
                              NULL, /* gcc_jit_location *loc */
                              gcc_jit_lvalue_as_rvalue (local_ival), /* gcc_jit_rvalue *rvalue */
                              type_unsigned_char); /* gcc_jit_type *type */
  gcc_jit_lvalue *dereference__dptr_0x18df2e0 =
    gcc_jit_rvalue_dereference (gcc_jit_lvalue_as_rvalue (local_dptr), /* gcc_jit_rvalue *rvalue */
                                NULL); /* gcc_jit_location *loc */
  gcc_jit_block_add_assignment (block_p_C1_end, /*gcc_jit_block *block */
                                NULL, /* gcc_jit_location *loc */
                                dereference__dptr_0x18df2e0, /* gcc_jit_lvalue *lvalue */
                                rvalue__unsigned_char_ival); /* gcc_jit_rvalue *rvalue */
  gcc_jit_rvalue *rvalue__int_1 =
    gcc_jit_context_new_rvalue_from_int (ctxt, /* gcc_jit_context *ctxt */
                                         type_int, /* gcc_jit_type *numeric_type */
                                         1); /* int value */
  gcc_jit_lvalue *lvalue_dptr__int_1_ = 
    gcc_jit_context_new_array_access (ctxt, /* gcc_jit_context *ctxt */
                                      NULL, /*gcc_jit_location *loc */
                                      gcc_jit_lvalue_as_rvalue (local_dptr), /* gcc_jit_rvalue *ptr */
                                      rvalue__int_1); /* gcc_jit_rvalue *index */
  gcc_jit_rvalue *address_of__dptr__int_1_ =
    gcc_jit_lvalue_get_address (lvalue_dptr__int_1_, /* gcc_jit_lvalue *lvalue */
                                NULL); /* gcc_jit_location *loc */
  gcc_jit_block_add_assignment (block_p_C1_end, /*gcc_jit_block *block */
                                NULL, /* gcc_jit_location *loc */
                                local_dptr, /* gcc_jit_lvalue *lvalue */
                                address_of__dptr__int_1_); /* gcc_jit_rvalue *rvalue */
  gcc_jit_rvalue *rvalue__int_1_0x18df500 =
    gcc_jit_context_new_rvalue_from_int (ctxt, /* gcc_jit_context *ctxt */
                                         type_int, /* gcc_jit_type *numeric_type */
                                         1); /* int value */
  gcc_jit_lvalue *lvalue_sptr__int_1_ = 
    gcc_jit_context_new_array_access (ctxt, /* gcc_jit_context *ctxt */
                                      NULL, /*gcc_jit_location *loc */
                                      gcc_jit_lvalue_as_rvalue (local_sptr), /* gcc_jit_rvalue *ptr */
                                      rvalue__int_1_0x18df500); /* gcc_jit_rvalue *index */
  gcc_jit_rvalue *address_of__sptr__int_1_ =
    gcc_jit_lvalue_get_address (lvalue_sptr__int_1_, /* gcc_jit_lvalue *lvalue */
                                NULL); /* gcc_jit_location *loc */
  gcc_jit_block_add_assignment (block_p_C1_end, /*gcc_jit_block *block */
                                NULL, /* gcc_jit_location *loc */
                                local_sptr, /* gcc_jit_lvalue *lvalue */
                                address_of__sptr__int_1_); /* gcc_jit_rvalue *rvalue */
  gcc_jit_rvalue *rvalue__int_1_0x18df650 =
    gcc_jit_context_new_rvalue_from_int (ctxt, /* gcc_jit_context *ctxt */
                                         type_int, /* gcc_jit_type *numeric_type */
                                         1); /* int value */
  gcc_jit_rvalue *rvalue_i____int_1 =
    gcc_jit_context_new_binary_op (ctxt,
                                   NULL, /* gcc_jit_location *loc */
                                   GCC_JIT_BINARY_OP_PLUS, /* enum gcc_jit_binary_op op */
                                   type_int, /* gcc_jit_type *result_type */
                                   gcc_jit_lvalue_as_rvalue (local_i), /* gcc_jit_rvalue *a */
                                   rvalue__int_1_0x18df650); /* gcc_jit_rvalue *b */
  gcc_jit_block_add_assignment (block_p_C1_end, /*gcc_jit_block *block */
                                NULL, /* gcc_jit_location *loc */
                                local_i, /* gcc_jit_lvalue *lvalue */
                                rvalue_i____int_1); /* gcc_jit_rvalue *rvalue */
  gcc_jit_block_end_with_jump (block_p_C1_end, /*gcc_jit_block *block */
                               NULL, /* gcc_jit_location *loc */
                               block_C2); /* gcc_jit_block *target */
  gcc_jit_rvalue *rvalue__int_1_0x18df7e0 =
    gcc_jit_context_new_rvalue_from_int (ctxt, /* gcc_jit_context *ctxt */
                                         type_int, /* gcc_jit_type *numeric_type */
                                         1); /* int value */
  gcc_jit_rvalue *rvalue_j____int_1 =
    gcc_jit_context_new_binary_op (ctxt,
                                   NULL, /* gcc_jit_location *loc */
                                   GCC_JIT_BINARY_OP_PLUS, /* enum gcc_jit_binary_op op */
                                   type_int, /* gcc_jit_type *result_type */
                                   gcc_jit_lvalue_as_rvalue (local_j), /* gcc_jit_rvalue *a */
                                   rvalue__int_1_0x18df7e0); /* gcc_jit_rvalue *b */
  gcc_jit_block_add_assignment (block_A2, /*gcc_jit_block *block */
                                NULL, /* gcc_jit_location *loc */
                                local_j, /* gcc_jit_lvalue *lvalue */
                                rvalue_j____int_1); /* gcc_jit_rvalue *rvalue */
  gcc_jit_block_end_with_jump (block_A2, /*gcc_jit_block *block */
                               NULL, /* gcc_jit_location *loc */
                               block_C1); /* gcc_jit_block *target */
  gcc_jit_block_end_with_void_return (block_A1, /*gcc_jit_block *block */
                                      NULL); /* gcc_jit_location *loc */
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  /* empty */
}
