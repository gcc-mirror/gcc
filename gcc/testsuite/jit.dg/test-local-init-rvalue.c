#include <stdio.h>
#include "libgccjit.h"
#include "harness.h"

/* This testcase checks that gcc_jit_context_new_constructor() works
   with locals. Tests that constructors can be used as return
   values or function call values. Test that constructors can have side
   effects and be assigned to locals.
 */

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  gcc_jit_type *int_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_INT);
  gcc_jit_type *pint_type = gcc_jit_type_get_pointer (int_type);
  gcc_jit_type *double_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_DOUBLE);
  gcc_jit_type *float_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_FLOAT);
  gcc_jit_type *bool_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_BOOL);
  gcc_jit_type *char_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_CHAR);
  gcc_jit_type *size_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_SIZE_T);
  gcc_jit_type *voidptr_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_VOID_PTR);
  gcc_jit_type *void_type = gcc_jit_context_get_type (ctxt,
    GCC_JIT_TYPE_VOID);

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
       int arr[50];
       float ff;
       char cc;
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
  gcc_jit_field *bar_cc = gcc_jit_context_new_field (ctxt,
				  0,
				  char_type,
				  "cc");
  gcc_jit_type *int50arr_type =
    gcc_jit_context_new_array_type (ctxt,
				    0,
				    int_type,
				    50);
  gcc_jit_field *bar_fi = gcc_jit_context_new_field (ctxt,
						 0,
						 int50arr_type,
						 "arr");
  gcc_jit_field *fields2[] = {bar_ff, bar_fi, bar_ii, bar_cc};

  gcc_jit_type *struct_bar_type =
    gcc_jit_struct_as_type (
      gcc_jit_context_new_struct_type (ctxt,
				       0,
				       "bar",
				       4,
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

  (void) ubar;
  (void) struct_bar_type;
  (void) struct_fi_type;
  (void) bool_type;
  (void) double_type;
  (void) pint_type;
  (void) voidptr_type;
  (void) size_type;

  gcc_jit_function *fn_int_3;
  { /* int foo () { int local = 3; return local;} */
    fn_int_3 =
      gcc_jit_context_new_function (ctxt,
				    0,
				    GCC_JIT_FUNCTION_EXPORTED,
				    int_type,
				    "fn_int_3",
				    0,
				    0,
				    0);
    gcc_jit_block *block = gcc_jit_function_new_block (fn_int_3, "start");
    gcc_jit_lvalue *local = gcc_jit_function_new_local (fn_int_3,
							0,
							int_type,
							"local");
    gcc_jit_rvalue *rval = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 3);

    gcc_jit_block_add_assignment (block, 0, local, rval);

    gcc_jit_block_end_with_return (block,
				   0,
				   gcc_jit_lvalue_as_rvalue(local));
  }
  { /* struct fi foo() { return (struct fi){1,2};} */
    gcc_jit_function *fn =
      gcc_jit_context_new_function (ctxt,
				    0,
				    GCC_JIT_FUNCTION_EXPORTED,
				    struct_fi_type,
				    "fn_fi_1_2",
				    0,
				    0,
				    0);
    gcc_jit_block *block = gcc_jit_function_new_block (fn, "start");

    gcc_jit_rvalue *rval_f1 = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 1);
    gcc_jit_rvalue *rval_i2 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 2);

    gcc_jit_rvalue *vals[] = { rval_f1, rval_i2};
    gcc_jit_field *fields[] = {fi_f, fi_i};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_struct_constructor
      (ctxt, 0,
       struct_fi_type,
       2,
       fields,
       vals);

    gcc_jit_block_end_with_return (block,
				   0,
				   ctor);
  }
  { /*
       struct fi foo()
       {
	 struct fi local = {1,2};
	 local = (struct fi){5,6};
	 return local;
       }
     */
    gcc_jit_function *fn =
      gcc_jit_context_new_function (ctxt,
				    0,
				    GCC_JIT_FUNCTION_EXPORTED,
				    struct_fi_type,
				    "fn_fi_5_6",
				    0,
				    0,
				    0);
    gcc_jit_lvalue *local = gcc_jit_function_new_local (fn,
							0,
							struct_fi_type,
							"local");
    gcc_jit_block *block = gcc_jit_function_new_block (fn, "start");

    {
      gcc_jit_rvalue *rval_f1 =
	gcc_jit_context_new_rvalue_from_int (ctxt, float_type, 1);
      gcc_jit_rvalue *rval_i2 =
	gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 2);

      gcc_jit_rvalue *vals[] = { rval_f1, rval_i2};
      gcc_jit_field *fields[] = {fi_f, fi_i};

      gcc_jit_rvalue *ctor = gcc_jit_context_new_struct_constructor
	(ctxt, 0,
	 struct_fi_type,
	 2,
	 fields,
	 vals);
      gcc_jit_block_add_assignment (block, 0, local, ctor);
    }
    {
      gcc_jit_rvalue *rval_f1 =
	gcc_jit_context_new_rvalue_from_int (ctxt, float_type, 5);
      gcc_jit_rvalue *rval_i2 =
	gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 6);

      gcc_jit_rvalue *vals[] = { rval_f1, rval_i2};
      gcc_jit_field *fields[] = {fi_f, fi_i};

      gcc_jit_rvalue *ctor = gcc_jit_context_new_struct_constructor
	(ctxt, 0,
	 struct_fi_type,
	 2,
	 fields,
	 vals);
      gcc_jit_block_add_assignment (block, 0, local, ctor);
    }

    gcc_jit_block_end_with_return (block,
				   0,
				   gcc_jit_lvalue_as_rvalue(local));
  }
  { /* struct fi foo() { struct fi local = {1, fn_int_3()};
			 return local;}

       The ctor has a side effect (funccall) */
    gcc_jit_function *fn =
      gcc_jit_context_new_function (ctxt,
				    0,
				    GCC_JIT_FUNCTION_EXPORTED,
				    struct_fi_type,
				    "fn_fi_1_3",
				    0,
				    0,
				    0);
    gcc_jit_lvalue *local = gcc_jit_function_new_local (fn,
							0,
							struct_fi_type,
							"local");
    gcc_jit_block *block = gcc_jit_function_new_block (fn, "start");

    {
      gcc_jit_rvalue *rval_f1 =
	gcc_jit_context_new_rvalue_from_int (ctxt, float_type, 1);
      gcc_jit_rvalue *rval_i2 =
	gcc_jit_context_new_call (ctxt, 0, fn_int_3, 0, 0);

      gcc_jit_rvalue *vals[] = { rval_f1, rval_i2};
      gcc_jit_field *fields[] = {fi_f, fi_i};

      gcc_jit_rvalue *ctor = gcc_jit_context_new_struct_constructor
	(ctxt, 0,
	 struct_fi_type,
	 2,
	 fields,
	 vals);
      gcc_jit_block_add_assignment (block, 0, local, ctor);
    }

    gcc_jit_block_end_with_return (block,
				   0,
				   gcc_jit_lvalue_as_rvalue(local));
  }
  { /* struct fi foo(fi) { return fi;}
       struct fi bar() { return foo((struct fi){3, 4}); }
     */

    gcc_jit_param *fi_param =
      gcc_jit_context_new_param (ctxt, 0, struct_fi_type, "fi");

    gcc_jit_function *fn0 =
      gcc_jit_context_new_function (ctxt,
				    0,
				    GCC_JIT_FUNCTION_EXPORTED,
				    struct_fi_type,
				    "fn_fi_x_x",
				    1,
				    &fi_param,
				    0);
    gcc_jit_block *block0 = gcc_jit_function_new_block (fn0, "start");
    gcc_jit_block_end_with_return (block0,
				   0,
				   gcc_jit_param_as_rvalue (
				     gcc_jit_function_get_param (fn0, 0)));

    gcc_jit_function *fn =
      gcc_jit_context_new_function (ctxt,
				    0,
				    GCC_JIT_FUNCTION_EXPORTED,
				    struct_fi_type,
				    "fn_fi_3_4",
				    0,
				    0,
				    0);
    gcc_jit_block *block = gcc_jit_function_new_block (fn, "start");

    gcc_jit_rvalue *rval_f1 = gcc_jit_context_new_rvalue_from_int (
      ctxt, float_type, 3);
    gcc_jit_rvalue *rval_i2 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 4);

    gcc_jit_rvalue *vals[] = { rval_f1, rval_i2};
    gcc_jit_field *fields[] = {fi_f, fi_i};

    gcc_jit_rvalue *ctor = gcc_jit_context_new_struct_constructor
      (ctxt, 0,
       struct_fi_type,
       2,
       fields,
       vals);

    gcc_jit_rvalue *call = gcc_jit_context_new_call (ctxt, 0, fn0, 1, &ctor);

    gcc_jit_block_end_with_return (block,
				   0,
				   call);
  }
  { /*
       void foo(struct bar *b) { *b = (struct bar) {.arr = {1,2}; }
     */

    gcc_jit_param *param =
      gcc_jit_context_new_param (ctxt, 0,
				 gcc_jit_type_get_pointer (struct_bar_type),
				 "b");


    gcc_jit_function *fn =
      gcc_jit_context_new_function (ctxt,
				    0,
				    GCC_JIT_FUNCTION_EXPORTED,
				    void_type,
				    "fn_pbar_12",
				    1,
				    &param,
				    0);
    gcc_jit_block *block = gcc_jit_function_new_block (fn, "start");

    gcc_jit_rvalue *rval_i1 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 1);
    gcc_jit_rvalue *rval_i2 = gcc_jit_context_new_rvalue_from_int (
      ctxt, int_type, 2);

    gcc_jit_rvalue *arr_vals[] = { rval_i1, rval_i2};

    gcc_jit_rvalue *arr_ctor = gcc_jit_context_new_array_constructor
      (ctxt, 0,
       int50arr_type,
       2,
       arr_vals);

    gcc_jit_rvalue *str_ctor = gcc_jit_context_new_struct_constructor
      (ctxt,
       0,
       struct_bar_type,
       1,
       &bar_fi,
       &arr_ctor);

    gcc_jit_param *p0 = gcc_jit_function_get_param (fn, 0);
    gcc_jit_lvalue *lv0 =  gcc_jit_param_as_lvalue (p0);
    gcc_jit_lvalue *deref =
      gcc_jit_rvalue_dereference (gcc_jit_lvalue_as_rvalue (lv0), 0);

    gcc_jit_block_add_assignment (block, 0,
				  deref,
				  str_ctor);

    gcc_jit_block_end_with_void_return (block, 0);
  }
  { /* struct bar foo() { struct bar local = {};
			  return local;}
     */
    gcc_jit_function *fn =
      gcc_jit_context_new_function (ctxt,
				    0,
				    GCC_JIT_FUNCTION_EXPORTED,
				    struct_bar_type,
				    "fn_bar_0s",
				    0,
				    0,
				    0);
    gcc_jit_lvalue *local =
      gcc_jit_function_new_local (fn,
				  0,
				  struct_bar_type,
				  "local");
    gcc_jit_block *block = gcc_jit_function_new_block (fn, "start");

    gcc_jit_rvalue *ctor = gcc_jit_context_new_struct_constructor
	(ctxt, 0,
	 struct_bar_type,
	 0,
	 0,
	 0);
    gcc_jit_block_add_assignment (block, 0, local, ctor);

    gcc_jit_block_end_with_return (block,
				   0,
				   gcc_jit_lvalue_as_rvalue(local));
  }
  { /* struct bar foo() { struct bar local;
			  local.arr = (int [50]){1,2,3,4,5,6};
			  return local;}
     */
    gcc_jit_function *fn =
      gcc_jit_context_new_function (ctxt,
				    0,
				    GCC_JIT_FUNCTION_EXPORTED,
				    struct_bar_type,
				    "fn_bar_123s",
				    0,
				    0,
				    0);
    gcc_jit_lvalue *local =
      gcc_jit_function_new_local (fn,
				  0,
				  struct_bar_type,
				  "local");
    gcc_jit_block *block = gcc_jit_function_new_block (fn, "start");

    gcc_jit_rvalue *values[6];

    for (int i = 0; i < 6; i++)
      values[i] = gcc_jit_context_new_rvalue_from_int (ctxt, int_type, i + 1);

    gcc_jit_rvalue *ctor = gcc_jit_context_new_array_constructor
	(ctxt, 0,
	 int50arr_type,
	 6,
	 values);

    gcc_jit_lvalue *arr_lv = gcc_jit_lvalue_access_field (local,
							  0,
							  bar_fi);
    gcc_jit_block_add_assignment (block, 0, arr_lv, ctor);

    gcc_jit_block_end_with_return (block,
				   0,
				   gcc_jit_lvalue_as_rvalue(local));
  }
  { /* int[50] foo() { int arr[50];
		       arr = (int [50]){1,2,3,4,5,6};
		       return arr;}

       N.B: Not a typo, returning an array.
     */
    gcc_jit_function *fn =
      gcc_jit_context_new_function (ctxt,
				    0,
				    GCC_JIT_FUNCTION_EXPORTED,
				    int50arr_type,
				    "fn_int50arr_123s",
				    0,
				    0,
				    0);
    gcc_jit_lvalue *local =
      gcc_jit_function_new_local (fn,
				  0,
				  int50arr_type,
				  "local");
    gcc_jit_block *block = gcc_jit_function_new_block (fn, "start");

    gcc_jit_rvalue *values[6];

    for (int i = 0; i < 6; i++)
      values[i] = gcc_jit_context_new_rvalue_from_int (ctxt, int_type, i + 1);

    gcc_jit_rvalue *ctor = gcc_jit_context_new_array_constructor (
	 ctxt,
	 0,
	 int50arr_type,
	 6,
	 values);

    gcc_jit_block_add_assignment (block, 0, local, ctor);

    gcc_jit_block_end_with_return (block,
				   0,
				   gcc_jit_lvalue_as_rvalue(local));
  }
  { /*
      Verify that circular linked lists compiles, .e.g.
      that visit_children does not run in circles or something.

      struct llist { struct llist *next; };

      bool foo (void)
      {
	volatile struct llist a;
	volatile struct llist b;

	a = (struct llist) {.next = &b};
	b = (struct llist) {.next = &a};

	return a.next == &b;
      }
    */
    gcc_jit_struct *llist =
      gcc_jit_context_new_opaque_struct(ctxt,
					0, "llist_lcl");
    gcc_jit_field *fields[] =
      {
	gcc_jit_context_new_field (ctxt, 0,
				   gcc_jit_type_get_pointer (
				     gcc_jit_struct_as_type (llist)),
				   "next")
      };
    gcc_jit_struct_set_fields (llist, 0, 1, fields);
    gcc_jit_type *t_llist = gcc_jit_struct_as_type (llist);

    gcc_jit_function *fn =
      gcc_jit_context_new_function (ctxt,
				    0,
				    GCC_JIT_FUNCTION_EXPORTED,
				    bool_type,
				    "fn_llist",
				    0,
				    0,
				    0);
    gcc_jit_block *block = gcc_jit_function_new_block (fn, "start");

    gcc_jit_lvalue *a =
      gcc_jit_function_new_local (fn,
				  0,
				  gcc_jit_type_get_volatile (t_llist),
				  "a");
    gcc_jit_lvalue *b =
      gcc_jit_function_new_local (fn,
				  0,
				  gcc_jit_type_get_volatile (t_llist),
				  "b");

    gcc_jit_rvalue *a_addr = gcc_jit_lvalue_get_address( a, 0);
    gcc_jit_rvalue *b_addr = gcc_jit_lvalue_get_address( b, 0);

    gcc_jit_rvalue *a_ctor = gcc_jit_context_new_struct_constructor (
	 ctxt,
	 0,
	 t_llist,
	 1,
	 0,
	 &b_addr);

    gcc_jit_rvalue *b_ctor = gcc_jit_context_new_struct_constructor (
	 ctxt,
	 0,
	 t_llist,
	 1,
	 0,
	 &a_addr);

    gcc_jit_block_add_assignment (block, 0,
				  a, a_ctor);
    gcc_jit_block_add_assignment (block, 0,
				  b, b_ctor);

    gcc_jit_rvalue *cmp =
      gcc_jit_context_new_comparison (
	ctxt, 0,
	GCC_JIT_COMPARISON_EQ,
	gcc_jit_rvalue_access_field (gcc_jit_lvalue_as_rvalue (a),
				     0, fields[0]),
	gcc_jit_context_new_cast (ctxt, 0,
				  gcc_jit_lvalue_get_address (b, 0),
				  gcc_jit_type_get_pointer (t_llist)));

    gcc_jit_block_end_with_return (block,
				   0, cmp);
  }
}

struct fi2 {
  float f;
  int i;
};

struct bar2 {
  float ff;
  int arr[50];
  int ii;
  char c;
};

union ubar2 {
  float ff;
  int ii;
};

struct int50arr {
  int arr[50];
};

void __attribute__((optimize(0)))
scramble_stack(void)
  {
    char *p = alloca(100);
    for (int i = 0; i < 100; i++)
      *p++ = 0xF0;
    asm(""); /* Mark for side-effect */
  }

void __attribute__((optimize(0)))
scramble_arr (char *arr, int len)
{
  for (int i = 0; i < len; i++)
    *arr++ = i;
  asm(""); /* Mark for side-effect */
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);

  {
    struct fi2 (*fn) (void) = gcc_jit_result_get_code (result, "fn_fi_1_2");
    scramble_stack ();
    struct fi2 fi = fn ();
    CHECK_VALUE (fi.f, 1);
    CHECK_VALUE (fi.i, 2);
  }
  {
    struct fi2 (*fn) (void) = gcc_jit_result_get_code (result, "fn_fi_5_6");
    struct fi2 fi = fn ();
    CHECK_VALUE (fi.f, 5);
    CHECK_VALUE (fi.i, 6);
  }
  {
    struct fi2 (*fn) (void) = gcc_jit_result_get_code (result, "fn_fi_1_3");
    struct fi2 fi = fn ();
    CHECK_VALUE (fi.f, 1);
    CHECK_VALUE (fi.i, 3);
  }
  {
    struct fi2 (*fn) (void) = gcc_jit_result_get_code (result, "fn_fi_3_4");
    struct fi2 fi = fn ();
    CHECK_VALUE (fi.f, 3);
    CHECK_VALUE (fi.i, 4);
  }
  {
    scramble_stack();
    struct bar2 (*fn) (void) = gcc_jit_result_get_code (result, "fn_bar_0s");
    struct bar2 bar = fn ();
    struct bar2 key = {};

    CHECK_VALUE (bar.ff, 0);
    CHECK_VALUE (bar.ii, 0);
    CHECK_VALUE (memcmp (&bar.arr, &key.arr, sizeof (key.arr)), 0);
  }
  {

    void (*fn) (struct bar2 *) = gcc_jit_result_get_code (result, "fn_pbar_12");

    struct bar2 bar = (struct bar2) {};

    scramble_arr ((char*)&bar, sizeof bar);
    scramble_stack();

    fn (&bar);

    struct bar2 key = {.arr = {1,2}};
    __builtin_clear_padding (&key);

    CHECK_VALUE (memcmp (&bar, &key, sizeof (key)), 0);
  }
  {
    scramble_stack();
    struct bar2 (*fn) (void) = gcc_jit_result_get_code (result, "fn_bar_123s");
    struct bar2 bar = fn ();
    struct bar2 key = {.arr = {1,2,3,4,5,6} };

    CHECK_VALUE (memcmp (&bar.arr, &key.arr, sizeof (key.arr)), 0);
  }
  {
    scramble_stack ();
    /* This is abit shady. Lets just pretend that array returns à la Fortran
       is the same thing as returning a struct with an array in it in C. */
    struct int50arr (*fn) (void) =
      gcc_jit_result_get_code (result, "fn_int50arr_123s");
    struct int50arr ans = fn ();
    int key[50] = {1,2,3,4,5,6};

    CHECK_VALUE (memcmp (ans.arr, key, sizeof (key)), 0);
  }
  {
    _Bool (*fn) (void) = gcc_jit_result_get_code (result, "fn_llist");
    CHECK_VALUE (fn (), 1);
  }
}
