#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#include "harness.h"

/* A doubly-linked list, to ensure that the JIT API can cope with
   self-referential types.  */
struct node
{
  struct node *prev;
  struct node *next;
  int value;
};

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
     int
     test_linked_list (struct node *n)
     {
	int total = 0;
	while (n)
	  {
	    total += n->value;
	    n = n->next;
	  }
	return total;
     }
  */
  gcc_jit_type *t_int =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_struct *t_node =
    gcc_jit_context_new_opaque_struct (ctxt, NULL, "node");
  gcc_jit_type *t_node_ptr =
    gcc_jit_type_get_pointer (gcc_jit_struct_as_type (t_node));

  gcc_jit_field *f_prev =
    gcc_jit_context_new_field (ctxt, NULL, t_node_ptr, "prev");
  gcc_jit_field *f_next =
    gcc_jit_context_new_field (ctxt, NULL, t_node_ptr, "next");
  gcc_jit_field *f_value =
    gcc_jit_context_new_field (ctxt, NULL, t_int, "value");
  gcc_jit_field *fields[] = {f_prev, f_next, f_value};
  gcc_jit_struct_set_fields (t_node, NULL, 3, fields);

  /* Build the test function.  */
  gcc_jit_param *param_n =
    gcc_jit_context_new_param (ctxt, NULL, t_node_ptr, "n");
  gcc_jit_function *fn =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  t_int,
				  "test_linked_list",
				  1, &param_n,
				  0);
  /* int total; */
  gcc_jit_lvalue *total =
    gcc_jit_function_new_local (fn, NULL, t_int, "total");

  gcc_jit_block *initial = gcc_jit_function_new_block (fn, "initial");
  gcc_jit_block *loop_test = gcc_jit_function_new_block (fn, "loop_test");
  gcc_jit_block *loop_body = gcc_jit_function_new_block (fn, "loop_body");
  gcc_jit_block *final = gcc_jit_function_new_block (fn, "final");

  /* total = 0; */
  gcc_jit_block_add_assignment (
    initial, NULL,
    total,
    gcc_jit_context_zero (ctxt, t_int));
  gcc_jit_block_end_with_jump (initial, NULL, loop_test);

  /* while (n) */
  gcc_jit_block_end_with_conditional (
    loop_test, NULL,
    gcc_jit_context_new_comparison (ctxt, NULL,
				    GCC_JIT_COMPARISON_NE,
				    gcc_jit_param_as_rvalue (param_n),
				    gcc_jit_context_null (ctxt, t_node_ptr)),
    loop_body,
    final);

  /* total += n->value; */
  gcc_jit_block_add_assignment_op (
    loop_body, NULL,
    total,
    GCC_JIT_BINARY_OP_PLUS,
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_rvalue_dereference_field (
	gcc_jit_param_as_rvalue (param_n),
	NULL,
	f_value)));

  /* n = n->next; */
  gcc_jit_block_add_assignment (
    loop_body, NULL,
    gcc_jit_param_as_lvalue (param_n),
    gcc_jit_lvalue_as_rvalue (
      gcc_jit_rvalue_dereference_field (
	gcc_jit_param_as_rvalue (param_n),
	NULL,
	f_next)));

  gcc_jit_block_end_with_jump (loop_body, NULL, loop_test);

  /* return total; */
  gcc_jit_block_end_with_return (
    final, NULL, gcc_jit_lvalue_as_rvalue (total));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  struct node a, b, c;
  typedef int (*fn_type) (struct node *n);
  CHECK_NON_NULL (result);

  fn_type test_linked_list =
    (fn_type)gcc_jit_result_get_code (result, "test_linked_list");
  CHECK_NON_NULL (test_linked_list);

  /* Construct a simple linked-list on the stack: a->b->c: */
  a.prev = NULL;
  a.next = &b;
  a.value = 5;

  b.prev = &a;
  b.next = &c;
  b.value = 3;

  c.prev = &b;
  c.next = NULL;
  c.value = 7;

  CHECK_VALUE (test_linked_list (NULL), 0);
  CHECK_VALUE (test_linked_list (&a), 15);
  CHECK_VALUE (test_linked_list (&b), 10);
  CHECK_VALUE (test_linked_list (&c), 7);
}
