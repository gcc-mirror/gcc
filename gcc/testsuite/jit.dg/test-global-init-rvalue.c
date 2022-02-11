/* This testcase checks that gcc_jit_global_set_initializer_rvalue() works
   with rvalues, especially with gcc_jit_context_new_*_constructor() for
   struct, unions and arrays. */

#include <stdio.h>
#include <string.h>

#include "libgccjit.h"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *int_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_INT);
  gcc_jit_type *short_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_SHORT);
  gcc_jit_type *pint_type = gcc_jit_type_get_pointer (int_type);
  gcc_jit_type *double_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_DOUBLE);
  gcc_jit_type *float_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_FLOAT);
  gcc_jit_type *bool_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_BOOL);
  gcc_jit_type *char_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_CHAR);
  gcc_jit_type *cpchar_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_CONST_CHAR_PTR);
  gcc_jit_type *size_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_SIZE_T);

  /* Make a struct: struct fi { float f; int i;} */
  gcc_jit_field *fi_f = gcc_jit_context_new_field (ctxt,
						   0,
						   float_type,
						   "f");
  gcc_jit_field *fi_i = gcc_jit_context_new_field (ctxt,
						   0,
						   int_type,
						   "i");
  gcc_jit_field *fields[] = {fi_f, fi_i};

  gcc_jit_type *struct_fi_type =
    gcc_jit_struct_as_type (
      gcc_jit_context_new_struct_type (ctxt,
				       0,
				       "fi",
				       2,
				       fields));

  /* Make a struct:

     struct bar {
       int ii;
       struct fi fi;
       float ff;
     }
  */
  gcc_jit_field *bar_ff = gcc_jit_context_new_field (ctxt,
						     0,
						     float_type,
						     "ff");
  gcc_jit_field *bar_ii = gcc_jit_context_new_field (ctxt,
						     0,
						     int_type,
						     "ii");
  gcc_jit_field *bar_fi = gcc_jit_context_new_field (ctxt,
						     0,
						     struct_fi_type,
						     "fi");
  gcc_jit_field *fields2[] = {bar_ff, bar_fi, bar_ii};

  gcc_jit_type *struct_bar_type =
    gcc_jit_struct_as_type (
      gcc_jit_context_new_struct_type (ctxt,
				       0,
				       "bar",
				       3,
				       fields2));

  /* Make an union:

     union ubar {
       float ff;
       int ii;
     };
  */
  gcc_jit_field *ubar_ff = gcc_jit_context_new_field (ctxt,
						      0,
						      float_type,
						      "ff");
  gcc_jit_field *ubar_ii = gcc_jit_context_new_field (ctxt,
						      0,
						      int_type,
						      "ii");
  gcc_jit_field *fields3[] = {ubar_ff, ubar_ii};

  gcc_jit_type *ubar = gcc_jit_context_new_union_type (ctxt,
						       0,
						       "ubar",
						       2,
						       fields3);

  { /* struct bar bar = {.ff=1, .fi={.f=2, .i=3}, .ii=4};
       I.e. nested ctors and with fields specified
     */
    gcc_jit_lvalue *bar = gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      struct_bar_type,
      "global_struct_bar_1234_1");

    gcc_jit_rvalue *fval = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 2);
    gcc_jit_rvalue *ival = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);

    gcc_jit_rvalue *vals[] = { fval, ival};
    gcc_jit_field *fields[] = {fi_f, fi_i};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_struct_constructor
      (ctxt, 0,
       struct_fi_type,
       2,
       fields,
       vals);

    ival = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 4);
    fval = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 1);

    gcc_jit_rvalue *vals2[] = {fval, ctor, ival};
    gcc_jit_field *fields2[] = {bar_ff, bar_fi, bar_ii};

    gcc_jit_rvalue *ctor_bar = gcc_jit_context_new_struct_constructor
      (ctxt, 0,
       struct_bar_type,
       3,
       fields2,
       vals2);

    gcc_jit_global_set_initializer_rvalue (bar, ctor_bar);
  }
  { /* struct bar bar = {1, {2, 3}, 4};
       I.e. nested ctors and fields implicit in definition order (fields=NULL)
     */
    gcc_jit_lvalue *bar = gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      struct_bar_type,
      "global_struct_bar_1234_2");

    gcc_jit_rvalue *fval = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 2);
    gcc_jit_rvalue *ival = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);

    gcc_jit_rvalue *vals[] = { fval, ival};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_struct_constructor
      (ctxt, 0,
       struct_fi_type,
       2,
       0,
       vals);

    ival = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 4);
    fval = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 1);

    gcc_jit_rvalue *vals2[] = {fval, ctor, ival};

    gcc_jit_rvalue *ctor_bar = gcc_jit_context_new_struct_constructor
      (ctxt, 0,
       struct_bar_type,
       3,
       0,
       vals2);

    gcc_jit_global_set_initializer_rvalue (bar, ctor_bar);
  }
  { /* struct fi foo = {.f=2, .i=3}; */
    gcc_jit_lvalue *foo = gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      struct_fi_type,
      "global_struct_fi_23_1");

    gcc_jit_rvalue *fval = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 2);
    gcc_jit_rvalue *ival = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);

    gcc_jit_rvalue *vals[] = { fval, ival};
    gcc_jit_field *fields[] = {fi_f, fi_i};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_struct_constructor
      (ctxt, 0,
       struct_fi_type,
       2,
       fields,
       vals);

    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* struct fi foo = {2, 3}; */
    gcc_jit_lvalue *foo = gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      struct_fi_type,
      "global_struct_fi_23_2");

    gcc_jit_rvalue *fval = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 2);
    gcc_jit_rvalue *ival = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);

    gcc_jit_rvalue *vals[] = { fval, ival};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_struct_constructor
      (ctxt, 0,
       struct_fi_type,
       2,
       0,
       vals);

    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* struct fi foo = {.i=0, .f=0}; (null init) */
    gcc_jit_lvalue *foo = gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      struct_fi_type,
      "global_struct_fi_00_1");

    gcc_jit_rvalue *vals[] = { 0, 0};
    gcc_jit_field *fields[] = {fi_f, fi_i};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_struct_constructor
      (ctxt, 0,
       struct_fi_type,
       2,
       fields,
       vals);

    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* struct fi foo = {0, 0}; (null fields, null elements in values) */
    gcc_jit_lvalue *foo = gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      struct_fi_type,
      "global_struct_fi_00_2");

    gcc_jit_rvalue *vals[] = { 0, 0};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_struct_constructor
      (ctxt, 0,
       struct_fi_type,
       2,
       0,
       vals);

    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* struct fi foo = {.i = 0} (null init);

       Null init values. */
    gcc_jit_lvalue *foo = gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      struct_fi_type,
      "global_struct_fi_0_1");

    gcc_jit_rvalue *vals[] = {0};
    gcc_jit_field *fields[] = {fi_i};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_struct_constructor
      (ctxt, 0,
       struct_fi_type,
       1,
       fields,
       vals);
    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* struct fi foo = {0};

       Null init values. */
    gcc_jit_lvalue *foo = gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      struct_fi_type,
      "global_struct_fi_0_2");

    gcc_jit_rvalue *vals[] = {0};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_struct_constructor
      (ctxt, 0,
       struct_fi_type,
       1,
       0,
       vals);
    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* struct fi foo = {}; (null init)

       Null fields and values. */
    gcc_jit_lvalue *foo = gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      struct_fi_type,
      "global_struct_fi_6");

    gcc_jit_rvalue *ctor = gcc_jit_context_new_struct_constructor
      (ctxt, 0,
       struct_fi_type,
       0,
       0,
       0);
    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* struct fi foo = {2 * 2, 3}; */
    gcc_jit_lvalue *foo = gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      struct_fi_type,
      "global_struct_fi_3");

    gcc_jit_rvalue *fval = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 2);
    gcc_jit_rvalue *fval2 = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 2);
    gcc_jit_rvalue *ival = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_rvalue *rval_mul = gcc_jit_context_new_binary_op (ctxt, 0,
      GCC_JIT_BINARY_OP_MULT,
      float_type,
      fval,
      fval2);

    gcc_jit_rvalue *vals[] = { rval_mul, ival};
    gcc_jit_field *fields[] = {fi_f, fi_i};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_struct_constructor
      (ctxt, 0,
       struct_fi_type,
       2,
       fields,
       vals);

    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* union ubar foo = {.ff = 3}; */
     gcc_jit_lvalue *foo = gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      ubar,
      "global_union_ufoo_ff3");

     gcc_jit_rvalue *fval = gcc_jit_context_new_rvalue_from_int (
       ctxt, float_type, 3);

     gcc_jit_rvalue *ctor = gcc_jit_context_new_union_constructor (
       ctxt,
       0,
       ubar,
       ubar_ff,
       fval);

     gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* union ubar foo = {.ii = 2}; */
     gcc_jit_lvalue *foo = gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      ubar,
      "global_union_ufoo_ii2");

     gcc_jit_rvalue *ival = gcc_jit_context_new_rvalue_from_int (
       ctxt, int_type, 2);

     gcc_jit_rvalue *ctor = gcc_jit_context_new_union_constructor (
       ctxt,
       0,
       ubar,
       ubar_ii,
       ival);

     gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* union ubar foo = {1.1f}; should init first field  */
     gcc_jit_lvalue *foo = gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      ubar,
      "global_union_ufoo_ff1c1");

     gcc_jit_rvalue *fval = gcc_jit_context_new_rvalue_from_double (
       ctxt, float_type, 1.1);

     gcc_jit_rvalue *ctor = gcc_jit_context_new_union_constructor (
       ctxt,
       0,
       ubar,
       0,
       fval);

     gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* union ubar foo = (union ubar){}; */
     gcc_jit_lvalue *foo = gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      ubar,
      "global_union_ufoo_0");

     gcc_jit_rvalue *ctor = gcc_jit_context_new_union_constructor (
       ctxt,
       0,
       ubar,
       0,
       0);

     gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* int foo = 3; */
    gcc_jit_rvalue *rval = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      int_type,
      "global_int1_3");
    gcc_jit_global_set_initializer_rvalue (foo,
      rval);
  }
  { /* const volatile int foo = 3; */
    gcc_jit_rvalue *rval = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      gcc_jit_type_get_const (gcc_jit_type_get_volatile (int_type)),
      "global_cvint1_3");
    gcc_jit_global_set_initializer_rvalue (foo,
      rval);
  }
  { /* Try the above, but with opposite order of global and literal calls */
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      int_type,
      "global_int2_3");
    gcc_jit_rvalue *rval = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_global_set_initializer_rvalue (foo,
      rval);
  }
  { /* int foo = 3 * (3 + 3) */
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      int_type,
      "global_int3_18");
    gcc_jit_rvalue *rval3_0 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_rvalue *rval3_1 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_rvalue *rval3_2 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_rvalue *rval_plus = gcc_jit_context_new_binary_op (ctxt, 0,
      GCC_JIT_BINARY_OP_PLUS,
      int_type,
      rval3_0,
      rval3_1);
    gcc_jit_rvalue *rval_mul = gcc_jit_context_new_binary_op (ctxt, 0,
      GCC_JIT_BINARY_OP_MULT,
      int_type,
      rval_plus,
      rval3_2);

    gcc_jit_global_set_initializer_rvalue (foo,
      rval_mul);
  }
  { /* int foo = ~(-(((((2 | 8) & 15) ^ 0) << 3 >> 2 - 1) / 2)); */
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      int_type,
      "global_int_alotofoperators");
    gcc_jit_rvalue *rval_0 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 2);
    gcc_jit_rvalue *rval_1 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 8);
    gcc_jit_rvalue *rval_2 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 15);
    gcc_jit_rvalue *rval_3 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 0);
    gcc_jit_rvalue *rval_4 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_rvalue *rval_5 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 2);
    gcc_jit_rvalue *rval_6 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 1);
    gcc_jit_rvalue *rval_7 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 2);

    gcc_jit_rvalue *rval_or = gcc_jit_context_new_binary_op (ctxt, 0,
      GCC_JIT_BINARY_OP_BITWISE_OR,
      int_type,
      rval_0,
      rval_1);
    gcc_jit_rvalue *rval_and = gcc_jit_context_new_binary_op (ctxt, 0,
      GCC_JIT_BINARY_OP_BITWISE_AND,
      int_type,
      rval_or,
      rval_2);
    gcc_jit_rvalue *rval_xor = gcc_jit_context_new_binary_op (ctxt, 0,
      GCC_JIT_BINARY_OP_BITWISE_XOR,
      int_type,
      rval_and,
      rval_3);
    gcc_jit_rvalue *rval_lsh = gcc_jit_context_new_binary_op (ctxt, 0,
      GCC_JIT_BINARY_OP_LSHIFT,
      int_type,
      rval_xor,
      rval_4);
    gcc_jit_rvalue *rval_rsh = gcc_jit_context_new_binary_op (ctxt, 0,
      GCC_JIT_BINARY_OP_RSHIFT,
      int_type,
      rval_lsh,
      rval_5);
    gcc_jit_rvalue *rval_min = gcc_jit_context_new_binary_op (ctxt, 0,
      GCC_JIT_BINARY_OP_MINUS,
      int_type,
      rval_rsh,
      rval_6);
    gcc_jit_rvalue *rval_div = gcc_jit_context_new_binary_op (ctxt, 0,
      GCC_JIT_BINARY_OP_DIVIDE,
      int_type,
      rval_min,
      rval_7);
    gcc_jit_rvalue *rval_umin =  gcc_jit_context_new_unary_op (ctxt, 0,
      GCC_JIT_UNARY_OP_MINUS,
      int_type,
      rval_div);
    gcc_jit_rvalue *rval_neg =  gcc_jit_context_new_unary_op (ctxt, 0,
      GCC_JIT_UNARY_OP_BITWISE_NEGATE,
      int_type,
      rval_umin);

    gcc_jit_global_set_initializer_rvalue (foo,
      rval_neg);
  }
  { /* int foo = 3; int *pfoo = &foo; */
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      int_type,
      "global_int4_3");
    gcc_jit_rvalue *rval = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_global_set_initializer_rvalue (foo,
      rval);

    gcc_jit_lvalue *pfoo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      pint_type,
      "global_pint5");
    gcc_jit_global_set_initializer_rvalue (pfoo,
      gcc_jit_lvalue_get_address (foo, 0));
  }
  { /* static int foo; int *pfoo = &foo; */
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_INTERNAL,
      int_type,
      "global_int5_3");

    gcc_jit_lvalue *pfoo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      pint_type,
      "global_pint6");
    gcc_jit_global_set_initializer_rvalue (pfoo,
      gcc_jit_lvalue_get_address (foo, 0));

    gcc_jit_function *fn =
      gcc_jit_context_new_function (ctxt,
				    0,
				    GCC_JIT_FUNCTION_EXPORTED,
				    gcc_jit_type_get_pointer(int_type),
				    "fn_pint_0",
				    0,
				    0,
				    0);

    gcc_jit_block *block = gcc_jit_function_new_block (fn, "start");

    gcc_jit_block_end_with_return (block,
				   0,
				   gcc_jit_lvalue_get_address (foo, 0));
  }
  { /* int foo = 3; int *pfoo = &foo + 1; */
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      int_type,
      "global_int6_3");
    gcc_jit_rvalue *rval = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_global_set_initializer_rvalue (foo,
      rval);

    gcc_jit_lvalue *pfoo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      pint_type,
      "global_pint7");
    gcc_jit_global_set_initializer_rvalue (pfoo,
      gcc_jit_lvalue_get_address (
	gcc_jit_context_new_array_access(
	  ctxt,
	  0,
	  gcc_jit_lvalue_get_address(foo, 0),
	  gcc_jit_context_one(ctxt, int_type)),
	0));
  }
  { /* double foo = 3; */
    gcc_jit_lvalue *double1 =  gcc_jit_context_new_global (
	ctxt, NULL,
	GCC_JIT_GLOBAL_EXPORTED,
	double_type,
	"global_double1_3");
    gcc_jit_rvalue *rval = gcc_jit_context_new_rvalue_from_int (
      ctxt, double_type, 3);
    gcc_jit_global_set_initializer_rvalue (double1,
      rval);
  }
  { /* double foo = 3 * 3 + 3 */
    gcc_jit_lvalue *double1 =  gcc_jit_context_new_global (
	ctxt, NULL,
	GCC_JIT_GLOBAL_EXPORTED,
	double_type,
	"global_double2_12");
    gcc_jit_rvalue *rval = gcc_jit_context_new_rvalue_from_int (
      ctxt, double_type, 3);
    gcc_jit_rvalue *rval_mul = gcc_jit_context_new_binary_op (ctxt, 0,
      GCC_JIT_BINARY_OP_MULT,
      double_type,
      rval,
      rval);
    gcc_jit_rvalue *rval_plus = gcc_jit_context_new_binary_op (ctxt, 0,
      GCC_JIT_BINARY_OP_PLUS,
      double_type,
      rval_mul,
      rval);
    gcc_jit_global_set_initializer_rvalue (double1,
      rval_plus);
  }
  { /* bool foo = 3 + 3 <= 6; */
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      bool_type,
      "global_bool1_1");
    gcc_jit_rvalue *rval3_0 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_rvalue *rval3_1 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_rvalue *rval6 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 6);
    gcc_jit_rvalue *rval_plus = gcc_jit_context_new_binary_op (ctxt,
      0,
      GCC_JIT_BINARY_OP_PLUS,
      int_type,
      rval3_0,
      rval3_1);
    gcc_jit_rvalue *rval_le = gcc_jit_context_new_comparison (ctxt,
      0,
      GCC_JIT_COMPARISON_LE,
      rval_plus,
      rval6);

    gcc_jit_global_set_initializer_rvalue (foo,
      rval_le);
  }
  gcc_jit_lvalue *global_intarr_1234;
  { /* int foo[] = {1,2,3,4}; */

    gcc_jit_type *arr_type = gcc_jit_context_new_array_type (ctxt,
							     0,
							     int_type,
							     4);
    gcc_jit_rvalue *rval_1 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 1);
    gcc_jit_rvalue *rval_2 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 2);
    gcc_jit_rvalue *rval_3 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_rvalue *rval_4 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 4);

    gcc_jit_rvalue *values[] = {rval_1, rval_2, rval_3, rval_4};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_array_constructor (ctxt,
								  0,
								  arr_type,
								  4,
								  values);
    global_intarr_1234 = gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      arr_type,
      "global_intarr_1234");
    gcc_jit_global_set_initializer_rvalue (global_intarr_1234, ctor);
  }
  { /* float foo[4] = {1,2}; */

    gcc_jit_type *arr_type = gcc_jit_context_new_array_type (ctxt,
							     0,
							     float_type,
							     4);
    gcc_jit_rvalue *rval_1 = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 1);
    gcc_jit_rvalue *rval_2 = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 2);

    gcc_jit_rvalue *values[] = {rval_1, rval_2};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_array_constructor (ctxt,
								  0,
								  arr_type,
								  2,
								  values);
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      arr_type,
      "global_floatarr_12");
    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* float foo[4] = {1,2};
       With different array objects of same size and type. */

    gcc_jit_type *arr_type = gcc_jit_context_new_array_type (ctxt,
							     0,
							     float_type,
							     4);
    gcc_jit_type *arr_type1 = gcc_jit_context_new_array_type (ctxt,
							      0,
							      float_type,
							      4);
    gcc_jit_rvalue *rval_1 = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 1);
    gcc_jit_rvalue *rval_2 = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 2);

    gcc_jit_rvalue *values[] = {rval_1, rval_2};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_array_constructor (ctxt,
								  0,
								  arr_type1,
								  2,
								  values);
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      arr_type,
      "global_floatarr_12_2");
    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* float foo[4] = {1,2,0}; (null init) */

    gcc_jit_type *arr_type = gcc_jit_context_new_array_type (ctxt,
							     0,
							     float_type,
							     4);
    gcc_jit_rvalue *rval_1 = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 1);
    gcc_jit_rvalue *rval_2 = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 2);

    gcc_jit_rvalue *values[] = {rval_1, rval_2, 0};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_array_constructor (ctxt,
								  0,
								  arr_type,
								  2,
								  values);
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      arr_type,
      "global_floatarr_120");
    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* float foo[4] = {}; (null init) */

    gcc_jit_type *arr_type = gcc_jit_context_new_array_type (ctxt,
							     0,
							     float_type,
							     4);

    gcc_jit_rvalue *ctor = gcc_jit_context_new_array_constructor (ctxt,
								  0,
								  arr_type,
								  0,
								  0);
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      arr_type,
      "global_floatarr_0000");
    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* float foo[4] = {NULL , NULL, 3, NULL, 5, 6}; (null init) */

    gcc_jit_type *arr_type = gcc_jit_context_new_array_type (ctxt,
							     0,
							     float_type,
							     8);
    gcc_jit_rvalue *rval3 = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 3);
    gcc_jit_rvalue *rval5 = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 5);
    gcc_jit_rvalue *rval6 = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 6);

    gcc_jit_rvalue *values[] = {0, 0, rval3, 0, rval5, rval6, 0 };

    gcc_jit_rvalue *ctor = gcc_jit_context_new_array_constructor (ctxt,
								  0,
								  arr_type,
								  7,
								  values);
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      arr_type,
      "global_floatarr_00305600");
    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* int *foo[4] = {0, &global_intarr_1234[1], 0}; */

    gcc_jit_type *arr_type = gcc_jit_context_new_array_type (ctxt,
							     0,
							     pint_type,
							     4);
    gcc_jit_rvalue *rval_1 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 1);
    gcc_jit_lvalue *arr_access = gcc_jit_context_new_array_access (
      ctxt,
      0,
      gcc_jit_lvalue_as_rvalue (global_intarr_1234),
      rval_1);
    gcc_jit_rvalue *rval_2 = gcc_jit_lvalue_get_address (arr_access, 0);
    gcc_jit_rvalue *rval_3 = gcc_jit_context_null (ctxt, pint_type);

    gcc_jit_rvalue *values[] = {0, rval_2, rval_3};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_array_constructor (ctxt,
								  0,
								  arr_type,
								  2,
								  values);
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      arr_type,
      "global_pintarr_x2xx");
    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* char foo[4] = {'q','w','e',0}; */

    gcc_jit_type *arr_type = gcc_jit_context_new_array_type (ctxt,
							     0,
							     char_type,
							     4);


    gcc_jit_rvalue *rvals[] = {
      gcc_jit_context_new_rvalue_from_int ( ctxt, char_type, 'q'),
      gcc_jit_context_new_rvalue_from_int ( ctxt, char_type, 'w'),
      gcc_jit_context_new_rvalue_from_int ( ctxt, char_type, 'e'),
      gcc_jit_context_new_rvalue_from_int ( ctxt, char_type, 0)
    };

    gcc_jit_rvalue *ctor = gcc_jit_context_new_array_constructor (ctxt,
								  0,
								  arr_type,
								  4,
								  rvals);
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      arr_type,
      "global_chararr_qwe");
    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* int foo[2][2] = {{1,2},{3,4}}; */

    gcc_jit_type *row_type = gcc_jit_context_new_array_type (ctxt,
							     0,
							     int_type,
							     2);

    gcc_jit_type *arr_type = gcc_jit_context_new_array_type (ctxt,
							     0,
							     row_type,
							     2);
    gcc_jit_rvalue *rvals_row0[] = {
      gcc_jit_context_new_rvalue_from_int ( ctxt, int_type, 1),
      gcc_jit_context_new_rvalue_from_int ( ctxt, int_type, 2)
    };
    gcc_jit_rvalue *rvals_row1[] = {
      gcc_jit_context_new_rvalue_from_int ( ctxt, int_type, 3),
      gcc_jit_context_new_rvalue_from_int ( ctxt, int_type, 4)
    };

    gcc_jit_rvalue *ctor_row0 =
      gcc_jit_context_new_array_constructor (ctxt,
					     0,
					     row_type,
					     2,
					     rvals_row0);
    gcc_jit_rvalue *ctor_row1 =
      gcc_jit_context_new_array_constructor (ctxt,
					     0,
					     row_type,
					     2,
					     rvals_row1);
    gcc_jit_rvalue *ctors_row[] = {ctor_row0, ctor_row1};

    gcc_jit_rvalue *ctor_arr =
      gcc_jit_context_new_array_constructor (ctxt,
					     0,
					     arr_type,
					     2,
					     ctors_row);

    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      arr_type,
      "global_int2x2matrix_1234");

    gcc_jit_global_set_initializer_rvalue (foo, ctor_arr);
  }
  { /* const char *foo[4] = {"qwe", "asd"}; */

    gcc_jit_type *arr_type = gcc_jit_context_new_array_type (ctxt,
							     0,
							     cpchar_type,
							     4);


    gcc_jit_rvalue *rvals[] = {
      gcc_jit_context_new_string_literal (ctxt, "qwe"),
      gcc_jit_context_new_string_literal (ctxt, "asd")
    };

    gcc_jit_rvalue *ctor = gcc_jit_context_new_array_constructor (ctxt,
								  0,
								  arr_type,
								  2,
								  rvals);
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      arr_type,
      "global_cpchararr_qwe_asd");
    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  { /* const int foo = 3;
       int bar = foo;
     */
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      gcc_jit_type_get_const (int_type),
      "global_const_int_3");
    gcc_jit_rvalue *rval3 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_global_set_initializer_rvalue (foo,
	rval3);
    gcc_jit_lvalue *bar =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      int_type,
      "global_lvalueinit_int_3");
    gcc_jit_global_set_initializer_rvalue (bar,
					   gcc_jit_lvalue_as_rvalue (foo));
  }
  { /* int foo = 3 * 2;
       int arr[] = {1,2,3,4};
       int *bar = &arr[2] + 1

       Example in the docs.
     */

    gcc_jit_type *arr_type = gcc_jit_context_new_array_type (ctxt,
							     0,
							     int_type,
							     4);
    gcc_jit_rvalue *rval_1 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 1);
    gcc_jit_rvalue *rval_2 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 2);
    gcc_jit_rvalue *rval_3 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_rvalue *rval_4 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 4);

    gcc_jit_rvalue *values[] = {rval_1, rval_2, rval_3, rval_4};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_array_constructor (ctxt,
								  0,
								  arr_type,
								  4,
								  values);
    gcc_jit_lvalue *global_intarr_1234 =
      gcc_jit_context_new_global (ctxt, NULL,
				  GCC_JIT_GLOBAL_EXPORTED,
				  arr_type,
				  "global_intarr_1234_2");

    gcc_jit_global_set_initializer_rvalue (global_intarr_1234, ctor);

    gcc_jit_lvalue *bar =
      gcc_jit_context_new_global (ctxt, NULL,
				  GCC_JIT_GLOBAL_EXPORTED,
				  int_type,
				  "global_int_6");
    gcc_jit_global_set_initializer_rvalue
      (bar,
       gcc_jit_context_new_binary_op
	 (ctxt, 0, GCC_JIT_BINARY_OP_MULT,
	  int_type,
	  gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 3),
	  gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 2)));

    gcc_jit_lvalue *pfoo =
      gcc_jit_context_new_global (ctxt, NULL,
				  GCC_JIT_GLOBAL_EXPORTED,
				  gcc_jit_type_get_pointer (int_type),
				  "global_pint_4");
    /* int *bar = &arr[2] + 1;

       In practice we could just do &foo[3]
       but just prove folding this works. */
    gcc_jit_global_set_initializer_rvalue (
       pfoo,
       gcc_jit_lvalue_get_address (
	 gcc_jit_context_new_array_access (
	   ctxt, 0,
	   gcc_jit_lvalue_get_address (
	     gcc_jit_context_new_array_access (
	       ctxt, 0,
	       gcc_jit_lvalue_as_rvalue (global_intarr_1234),
	       gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 2)),
	       0),
	   gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 1)),
	   0));
  }
  { /*  static int bar = 11;
	int foo () { return bar; } */

    gcc_jit_lvalue *bar =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_INTERNAL,
      int_type,
      "global_static_int_11");
    gcc_jit_rvalue *rval1 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 11);
    gcc_jit_global_set_initializer_rvalue (bar,
	rval1);

    gcc_jit_function *fn11 =
      gcc_jit_context_new_function (ctxt,
				    0,
				    GCC_JIT_FUNCTION_EXPORTED,
				    int_type,
				    "fn_int_11",
				    0,
				    0,
				    0);
    gcc_jit_block *block = gcc_jit_function_new_block (fn11, "start");

    gcc_jit_block_end_with_return (block,
				   0,
				   gcc_jit_lvalue_as_rvalue(bar));
  }
  { /* static const int cbar = 11;
       int cfoo () { return cbar; } */

    gcc_jit_lvalue *bar =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_INTERNAL,
      gcc_jit_type_get_const (int_type),
      "global_static_cint_11");
    gcc_jit_rvalue *rval1 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 11);
    gcc_jit_global_set_initializer_rvalue (bar,
	rval1);

    gcc_jit_function *fn11 =
      gcc_jit_context_new_function (ctxt,
				    0,
				    GCC_JIT_FUNCTION_EXPORTED,
				    int_type,
				    "fn_cint_11",
				    0,
				    0,
				    0);
    gcc_jit_block *block = gcc_jit_function_new_block (fn11, "start");

    gcc_jit_block_end_with_return (block,
				   0,
				   gcc_jit_lvalue_as_rvalue(bar));
  }
  { /* static const int cbar = 12;
       const int* cfoo () { return &cbar; } */

    gcc_jit_lvalue *bar =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_INTERNAL,
      gcc_jit_type_get_const (int_type),
      "global_static_cint_12");
    gcc_jit_rvalue *rval1 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 12);
    gcc_jit_global_set_initializer_rvalue (bar,
	rval1);

    gcc_jit_function *fn11 =
      gcc_jit_context_new_function (ctxt,
				    0,
				    GCC_JIT_FUNCTION_EXPORTED,
				    gcc_jit_type_get_pointer(int_type),
				    "fn_cint_12",
				    0,
				    0,
				    0);

    gcc_jit_block *block = gcc_jit_function_new_block (fn11, "start");

    gcc_jit_block_end_with_return (block,
				   0,
				   gcc_jit_lvalue_get_address (bar, 0));
  }
  { /* const int foo = 3;
       short bar = (short)foo;

       Assure casts fold
     */
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      gcc_jit_type_get_const (int_type),
      "global_const_int_4");
    gcc_jit_rvalue *rval3 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_global_set_initializer_rvalue (foo,
	rval3);
    gcc_jit_lvalue *bar =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      short_type,
      "global_lvalueinit_short_3");
    gcc_jit_global_set_initializer_rvalue (
      bar,
      gcc_jit_context_new_cast( ctxt, 0,
				gcc_jit_lvalue_as_rvalue (foo),
				short_type));
  }
  { /* const int foo = 3;
       const int const *bar = &foo; */
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      gcc_jit_type_get_const (int_type),
      "global_const_int_6");
    gcc_jit_rvalue *rval3 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);
    gcc_jit_global_set_initializer_rvalue (foo,
	rval3);
    gcc_jit_lvalue *bar =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      gcc_jit_type_get_const (
	gcc_jit_type_get_pointer (
	  gcc_jit_type_get_const (
	    int_type))),
      "global_lvalueinit_cpcint_3");
    gcc_jit_global_set_initializer_rvalue (
      bar,
      gcc_jit_lvalue_get_address (foo, 0));
  }
  { /* const int __attribute__ ((aligned (64))) foo = 3;
       int bar = foo;

       Assure alignement does not make the constant "miss"
       or something strange.
     */
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      gcc_jit_type_get_const (gcc_jit_type_get_aligned (int_type, 64)),
      "global_const_int_7");
    gcc_jit_rvalue *rval = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 4);
    gcc_jit_global_set_initializer_rvalue (foo,
	rval);
    gcc_jit_lvalue *bar =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      int_type,
      "global_lvalueinit_int_4");
    gcc_jit_global_set_initializer_rvalue (bar,
					   gcc_jit_lvalue_as_rvalue (foo));
  }
  {
    /* union upintsize { size_t s; int *p } u = {.s = 0xEEEFBEEF}; */
    gcc_jit_field *f1 = gcc_jit_context_new_field (ctxt,
						   0,
						   size_type,
						   "s");
    gcc_jit_field *f2 = gcc_jit_context_new_field (ctxt,
						   0,
						   pint_type,
						   "p");
    gcc_jit_field *fields1[] = {f1, f2};

    gcc_jit_type *ubar = gcc_jit_context_new_union_type (ctxt,
							 0,
							 "upintsize",
							 2,
							 fields1);
    gcc_jit_lvalue *foo =  gcc_jit_context_new_global (
      ctxt, NULL,
      GCC_JIT_GLOBAL_EXPORTED,
      gcc_jit_type_get_const (ubar),
      "global_const_upintsize_1");

    gcc_jit_rvalue *val = gcc_jit_context_new_rvalue_from_long (
      ctxt, size_type, 0xEEEFBEEF);

    gcc_jit_rvalue *ctor =
      gcc_jit_context_new_union_constructor (ctxt,
					     0,
					     ubar,
					     f1,
					     val);

    gcc_jit_global_set_initializer_rvalue (foo, ctor);
  }
  {/*
      struct B;
      struct A { B* b; };
      struct B { A* a; };
      extern struct B b;
      struct A a = {.b = b};
      struct B b = {.a = a};

      See that opaque structs and circular pointers works.
   */

    gcc_jit_struct *struct_B =
      gcc_jit_context_new_opaque_struct(ctxt,
					0, "B");

    gcc_jit_field *fields_A[] =
      {
	gcc_jit_context_new_field (ctxt, 0,
				   gcc_jit_type_get_pointer (
				     gcc_jit_struct_as_type (struct_B)),
				   "b")
      };

    gcc_jit_struct *struct_A =
      gcc_jit_context_new_struct_type(ctxt, 0, "A", 1, fields_A);

    gcc_jit_field *fields_B[] =
      {
	gcc_jit_context_new_field (ctxt, 0,
				   gcc_jit_type_get_pointer (
				     gcc_jit_struct_as_type (struct_A)),
				   "a")
      };

    gcc_jit_struct_set_fields (struct_B, 0, 1, fields_B);

    gcc_jit_lvalue *a =
      gcc_jit_context_new_global (ctxt, 0, GCC_JIT_GLOBAL_EXPORTED,
				  gcc_jit_struct_as_type (struct_A),
				  "a_glb");
    gcc_jit_lvalue *b =
      gcc_jit_context_new_global (ctxt, 0, GCC_JIT_GLOBAL_EXPORTED,
				  gcc_jit_struct_as_type (struct_B),
				  "b_glb");
    gcc_jit_rvalue *b_addr = gcc_jit_lvalue_get_address( b, 0);
    gcc_jit_rvalue *a_ctor =
      gcc_jit_context_new_struct_constructor (ctxt, 0,
					      gcc_jit_struct_as_type (struct_A),
					      1, 0,
					      &b_addr);
    gcc_jit_rvalue *a_addr = gcc_jit_lvalue_get_address( a, 0);
    gcc_jit_rvalue *b_ctor =
      gcc_jit_context_new_struct_constructor (ctxt, 0,
					      gcc_jit_struct_as_type (struct_B),
					      1, 0,
					      &a_addr);

    gcc_jit_global_set_initializer_rvalue(a, a_ctor);
    gcc_jit_global_set_initializer_rvalue(b, b_ctor);
  }
}

struct fi {
  float f;
  int i;
};

struct bar1 {
  float ff;
  struct fi fi;
  int ii;
};

union ubar1 {
  float ff;
  int ii;
};

union upintsize {
  size_t s;
  int *p;
};

struct B_glb;
struct A_glb {
  struct B_glb *b;
};
struct B_glb {
  struct A_glb *a;
};

int test_aligned64_works_in_linker_1 __attribute__ ((aligned (64))) = 0;
int test_aligned64_works_in_linker_2 __attribute__ ((aligned (64))) = 0;

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);

  {
    struct bar1 *bar =
      gcc_jit_result_get_global (result, "global_struct_bar_1234_1");

    CHECK_VALUE (bar->ff, 1);
    CHECK_VALUE (bar->fi.f, 2);
    CHECK_VALUE (bar->fi.i, 3);
    CHECK_VALUE (bar->ii, 4);
  }
  {
    struct bar1 *bar =
      gcc_jit_result_get_global (result, "global_struct_bar_1234_2");

    CHECK_VALUE (bar->ff, 1);
    CHECK_VALUE (bar->fi.f, 2);
    CHECK_VALUE (bar->fi.i, 3);
    CHECK_VALUE (bar->ii, 4);
  }
  {
    struct fi *fi = gcc_jit_result_get_global (result, "global_struct_fi_23_1");

    CHECK_VALUE (fi->f, 2);
    CHECK_VALUE (fi->i, 3);
  }
  {
    struct fi *fi = gcc_jit_result_get_global (result, "global_struct_fi_23_2");

    CHECK_VALUE (fi->f, 2);
    CHECK_VALUE (fi->i, 3);
  }
  {
    struct fi *fi = gcc_jit_result_get_global (result, "global_struct_fi_00_1");

    CHECK_VALUE (fi->f, 0);
    CHECK_VALUE (fi->i, 0);
  }
  {
    struct fi *fi = gcc_jit_result_get_global (result, "global_struct_fi_00_2");

    CHECK_VALUE (fi->f, 0);
    CHECK_VALUE (fi->i, 0);
  }
  {
    struct fi *fi = gcc_jit_result_get_global (result, "global_struct_fi_0_1");

    CHECK_VALUE (fi->f, 0);
    CHECK_VALUE (fi->i, 0);
  }
  {
    struct fi *fi = gcc_jit_result_get_global (result, "global_struct_fi_0_2");

    CHECK_VALUE (fi->f, 0);
    CHECK_VALUE (fi->i, 0);
  }
  {
    struct fi *fi = gcc_jit_result_get_global (result, "global_struct_fi_6");

    CHECK_VALUE (fi->f, 0);
    CHECK_VALUE (fi->i, 0);
  }
  {
    struct fi *fi = gcc_jit_result_get_global (result, "global_struct_fi_3");

    CHECK_VALUE (fi->f, 2 * 2);
    CHECK_VALUE (fi->i, 3);
  }
  {
    union ubar1 *foo = gcc_jit_result_get_global (result,
						  "global_union_ufoo_ff3");
    CHECK_VALUE (foo->ff, 3);
  }
  {
    union ubar1 *foo = gcc_jit_result_get_global (result,
						  "global_union_ufoo_ii2");
    CHECK_VALUE (foo->ii, 2);
  }
  {
    union ubar1 *foo = gcc_jit_result_get_global (result,
						  "global_union_ufoo_ff1c1");
    CHECK_VALUE (foo->ff, 1.1f);
  }
  {
    union ubar1 *foo = gcc_jit_result_get_global (result,
						  "global_union_ufoo_0");
    CHECK_VALUE (foo->ii, 0);
  }
  {
    int *foo = gcc_jit_result_get_global (result, "global_int1_3");

    CHECK_VALUE (*foo, 3);
  }
  {
    int *foo = gcc_jit_result_get_global (result, "global_cvint1_3");

    CHECK_VALUE (*foo, 3);
  }
  {
    int *foo = gcc_jit_result_get_global (result, "global_int2_3");

    CHECK_VALUE (*foo, 3);
  }
  {
    int *foo = gcc_jit_result_get_global (result, "global_int3_18");

    CHECK_VALUE (*foo, 18);
  }
  {
    int *foo = gcc_jit_result_get_global (result, "global_int_alotofoperators");

    CHECK_VALUE (*foo, ~(-((((((2 | 8) & 15) ^ 0) << 3 >> 2) - 1) / 2)));
  }
  {
    int *foo = gcc_jit_result_get_global (result, "global_int4_3");
    int **pfoo = gcc_jit_result_get_global (result, "global_pint5");

    CHECK_VALUE (*foo, 3);
    CHECK_VALUE (foo, *pfoo);
    CHECK_VALUE (**pfoo, 3);
  }
  {
    int * (*foo) (void) = gcc_jit_result_get_code (result, "fn_pint_0");
    int **pfoo = gcc_jit_result_get_global (result, "global_pint6");

    CHECK_VALUE (*foo (), 0);
    CHECK_VALUE (foo (), *pfoo);
    CHECK_VALUE (**pfoo, 0);
  }
  {
    int *foo = gcc_jit_result_get_global (result, "global_int6_3");
    int **pfoo = gcc_jit_result_get_global (result, "global_pint7");

    CHECK_VALUE (*foo, 3);
    CHECK_VALUE (foo + 1, *pfoo);
    CHECK_VALUE (*(*pfoo - 1), 3);
  }
  {
    double *foo = gcc_jit_result_get_global (result, "global_double1_3");

    CHECK_VALUE (*foo, 3);
  }
  {
    double *foo = gcc_jit_result_get_global (result, "global_double2_12");

    CHECK_VALUE (*foo, 12);
  }
  {
    _Bool *foo = gcc_jit_result_get_global (result, "global_bool1_1");

    CHECK_VALUE (*foo, 1);
  }
  {
    int *foo = gcc_jit_result_get_global (result, "global_intarr_1234");

    CHECK_VALUE (foo[0], 1);
    CHECK_VALUE (foo[1], 2);
    CHECK_VALUE (foo[2], 3);
    CHECK_VALUE (foo[3], 4);
  }
  {
    float *foo = gcc_jit_result_get_global (result, "global_floatarr_12");

    CHECK_VALUE (foo[0], 1);
    CHECK_VALUE (foo[1], 2);
    CHECK_VALUE (foo[2], 0);
    CHECK_VALUE (foo[3], 0);
  }
  {
    float *foo = gcc_jit_result_get_global (result, "global_floatarr_12_2");

    CHECK_VALUE (foo[0], 1);
    CHECK_VALUE (foo[1], 2);
    CHECK_VALUE (foo[2], 0);
    CHECK_VALUE (foo[3], 0);
  }
  {
    float *foo = gcc_jit_result_get_global (result, "global_floatarr_120");

    CHECK_VALUE (foo[0], 1);
    CHECK_VALUE (foo[1], 2);
    CHECK_VALUE (foo[2], 0);
    CHECK_VALUE (foo[3], 0);
  }
  {
    float *foo = gcc_jit_result_get_global (result, "global_floatarr_0000");

    CHECK_VALUE (foo[0], 0);
    CHECK_VALUE (foo[1], 0);
    CHECK_VALUE (foo[2], 0);
    CHECK_VALUE (foo[3], 0);
  }
  {
    float *foo = gcc_jit_result_get_global (result, "global_floatarr_00305600");

    float key[] = {0,0,3,0,5,6,0,0};

    CHECK_VALUE (memcmp (foo, key, sizeof key), 0);
  }
  {
    int **foo = gcc_jit_result_get_global (result, "global_pintarr_x2xx");

    CHECK_VALUE (foo[0], 0);
    CHECK_VALUE (*foo[1], 2);
  }
  {
    char *foo = gcc_jit_result_get_global (result, "global_chararr_qwe");
    const char *key = "qwe";
    CHECK_VALUE (strcmp (foo, key), 0);
  }
  {
    int *foo = gcc_jit_result_get_global (result, "global_int2x2matrix_1234");

    for (int i = 0; i < 4; i++)
      CHECK_VALUE (foo[i], i + 1);
  }
  {
    const char **foo =
      gcc_jit_result_get_global (result, "global_cpchararr_qwe_asd");

    CHECK_VALUE (strcmp (foo[0], "qwe"), 0);
    CHECK_VALUE (strcmp (foo[1], "asd"), 0);
  }
  {
    int *foo = gcc_jit_result_get_global (result, "global_lvalueinit_int_3");

    CHECK_VALUE (*foo, 3);
  }
  {
    int **pint =
      gcc_jit_result_get_global (result, "global_pint_4");
    int *foo =
      gcc_jit_result_get_global (result, "global_int_6");
    CHECK_VALUE (**pint, 4);
    CHECK_VALUE (*foo, 6);
  }
  {
    int (*fn)(void) = gcc_jit_result_get_code (result, "fn_int_11");
    CHECK_VALUE (fn (), 11);
  }
  {
    int (*fn)(void) = gcc_jit_result_get_code (result, "fn_cint_11");
    CHECK_VALUE (fn (), 11);
  }
  {
    int *(*fn)(void) = gcc_jit_result_get_code (result, "fn_cint_12");
    CHECK_VALUE (*fn (), 12);
  }
  {
    short *foo =
      gcc_jit_result_get_code (result, "global_lvalueinit_short_3");
    CHECK_VALUE (*foo, 3);
  }
  {
    int **foo =
      gcc_jit_result_get_code (result, "global_lvalueinit_cpcint_3");
    CHECK_VALUE (**foo, 3);
  }
  {
    int *foo =
      gcc_jit_result_get_code (result, "global_lvalueinit_int_4");
    CHECK_VALUE (*foo, 4);

    int *bar =
      gcc_jit_result_get_code (result, "global_const_int_7");
    CHECK_VALUE (*bar, 4);
    /* The linker does not have to support up to 64 alignment, so test that
       it does before testing that it works in libgccjit. */
    if ((size_t) &test_aligned64_works_in_linker_1 % 64 == 0 &&
	(size_t) &test_aligned64_works_in_linker_2 % 64 == 0)
      CHECK_VALUE ((size_t) bar % 64, 0); /* __attribute__ ((aligned (64))) */
  }
  {
    union upintsize *foo =
      gcc_jit_result_get_code (result, "global_const_upintsize_1");
    CHECK_VALUE (foo->p, (void*)0xEEEFBEEF);
  }
  {
    struct A_glb *a =
      gcc_jit_result_get_code (result, "a_glb");
    struct B_glb *b =
      gcc_jit_result_get_code (result, "b_glb");

    CHECK_VALUE (a->b, b);
    CHECK_VALUE (b->a, a);
  }
}
