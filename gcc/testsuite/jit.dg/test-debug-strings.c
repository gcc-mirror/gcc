#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

/* Build various compound expressions, and verify that they have sane debug
   strings.  */
void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Make a singly-linked list type:
      struct node
      {
       struct node *next;
       int value;
      };
  */
  gcc_jit_type *t_int =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_struct *t_node =
    gcc_jit_context_new_opaque_struct (ctxt, NULL, "node");
  gcc_jit_type *t_node_ptr =
    gcc_jit_type_get_pointer (gcc_jit_struct_as_type (t_node));
  gcc_jit_field *f_next =
    gcc_jit_context_new_field (ctxt, NULL, t_node_ptr, "next");
  gcc_jit_field *f_value =
    gcc_jit_context_new_field (ctxt, NULL, t_int, "value");
  gcc_jit_field *fields[] = {f_next, f_value};
  gcc_jit_struct_set_fields (t_node, NULL, 2, fields);

  /* Create a dummy function so that we have locals/params to build
     expressions with.  */
  gcc_jit_type *t_void =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_function *fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  t_void,
				  "test_debug_strings",
				  0, NULL, 0);
  gcc_jit_rvalue *ptr =
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_function_new_local (fn,
				  NULL,
				  t_node_ptr,
				  "ptr"));
  gcc_jit_rvalue *a =
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_function_new_local (fn, NULL, t_int, "a"));
  gcc_jit_rvalue *b =
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_function_new_local (fn, NULL, t_int, "b"));
  gcc_jit_rvalue *c =
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_function_new_local (fn, NULL, t_int, "c"));
  gcc_jit_rvalue *d =
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_function_new_local (fn, NULL, t_int, "d"));

#define CHECK_RVALUE_DEBUG_STRING(RVALUE, EXPECTED) \
  CHECK_STRING_VALUE ( \
    gcc_jit_object_get_debug_string (gcc_jit_rvalue_as_object (RVALUE)), \
    (EXPECTED))

#define CHECK_LVALUE_DEBUG_STRING(LVALUE, EXPECTED) \
  CHECK_STRING_VALUE ( \
    gcc_jit_object_get_debug_string (gcc_jit_lvalue_as_object (LVALUE)), \
    (EXPECTED))

  /* Verify various simple compound expressions.  */
  {
    CHECK_RVALUE_DEBUG_STRING (ptr, "ptr");

    gcc_jit_lvalue *deref =
      gcc_jit_rvalue_dereference_field (ptr,
					NULL,
					f_value);
    CHECK_LVALUE_DEBUG_STRING (deref, "ptr->value");

    gcc_jit_rvalue *deref_as_rvalue = gcc_jit_lvalue_as_rvalue (deref);

#define BINOP(OP, A, B) \
    gcc_jit_context_new_binary_op (ctxt, NULL, \
				   GCC_JIT_BINARY_OP_##OP, t_int, (A), (B))
#define COMPARISON(OP, A, B) \
    gcc_jit_context_new_comparison (ctxt, NULL, \
				    GCC_JIT_COMPARISON_##OP,(A), (B))

    CHECK_RVALUE_DEBUG_STRING (
      BINOP (PLUS, deref_as_rvalue, deref_as_rvalue),
      "ptr->value + ptr->value");
    CHECK_RVALUE_DEBUG_STRING (
      BINOP (MULT, deref_as_rvalue, deref_as_rvalue),
      "ptr->value * ptr->value");

   /* Multiplication has higher precedence in C than addition, so this
       dump shouldn't contain parentheses.  */
    CHECK_RVALUE_DEBUG_STRING (
      BINOP (PLUS,
	     BINOP (MULT, a, b),
	     BINOP (MULT, c, d)),
      "a * b + c * d");

    /* ...but this one should.  */
    CHECK_RVALUE_DEBUG_STRING (
      BINOP (MULT,
	     BINOP (PLUS, a, b),
	     BINOP (PLUS, c, d)),
      "(a + b) * (c + d)");

    /* Equal precedences don't need parentheses.  */
    CHECK_RVALUE_DEBUG_STRING (
      BINOP (MULT,
	     BINOP (MULT, a, b),
	     BINOP (MULT, c, d)),
      "a * b * c * d");

    /* Comparisons and logical ops.  */
    CHECK_RVALUE_DEBUG_STRING (
      COMPARISON (LT, a, b),
      "a < b");

    CHECK_RVALUE_DEBUG_STRING (
      BINOP (LOGICAL_AND,
	     COMPARISON (LT, a, b),
	     COMPARISON (GT, c, d)),
      "a < b && c > d");

    CHECK_RVALUE_DEBUG_STRING (
      BINOP (LOGICAL_AND,
	     BINOP (LOGICAL_OR,
		    COMPARISON (LT, a, b),
		    COMPARISON (LT, a, c)),
	     BINOP (LOGICAL_OR,
		    COMPARISON (GT, d, b),
		    COMPARISON (GT, d, c))),
      "(a < b || a < c) && (d > b || d > c)");

    CHECK_RVALUE_DEBUG_STRING (
      BINOP (LOGICAL_OR,
	     BINOP (LOGICAL_AND,
		    COMPARISON (LT, a, b),
		    COMPARISON (LT, a, c)),
	     BINOP (LOGICAL_AND,
		    COMPARISON (GT, d, b),
		    COMPARISON (GT, d, c))),
      "a < b && a < c || d > b && d > c");

#undef BINOP
#undef COMPARISON
  }

  /* PR jit/66539 "Missing parentheses in jit dumps".
     Construct the equivalent of
       ((cast)ptr->next)->next
     and verify that the appropriate parentheses appear in the debug
     string.   */
  {
    /* "ptr->next". */
    gcc_jit_lvalue *inner_deref =
      gcc_jit_rvalue_dereference_field (ptr,
					NULL,
					f_next);
    /* "((node *)ptr->next)"; the cast is redundant, purely
       to exercise dumping.  */
    gcc_jit_rvalue *test_cast =
      gcc_jit_context_new_cast (ctxt, NULL,
				gcc_jit_lvalue_as_rvalue (inner_deref),
				t_node_ptr);
    /* "((node *)ptr->next)->next".  */
    gcc_jit_lvalue *outer_deref =
      gcc_jit_rvalue_dereference_field (test_cast, /* gcc_jit_rvalue *ptr */
					NULL, /* gcc_jit_location *loc */
					f_next); /* gcc_jit_field *field */
    CHECK_LVALUE_DEBUG_STRING (outer_deref,
			       "((struct node *)ptr->next)->next");
  }

#undef CHECK_LVALUE_DEBUG_STRING
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);
  /* We don't actually build any functions above;
     nothing more to verify.  */
}
