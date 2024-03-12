/* { dg-do compile { target x86_64-*-* } } */

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

/* We don't want set_options() in harness.h to set -O3 to see that the restrict
	 attribute affects the optimizations. */
#define TEST_ESCHEWS_SET_OPTIONS
static void set_options (gcc_jit_context *ctxt, const char *argv0)
{
	// Set "-O3".
	gcc_jit_context_set_int_option(ctxt, GCC_JIT_INT_OPTION_OPTIMIZATION_LEVEL, 3);
}

#define TEST_COMPILING_TO_FILE
#define OUTPUT_KIND      GCC_JIT_OUTPUT_KIND_ASSEMBLER
#define OUTPUT_FILENAME  "output-of-test-restrict.c.s"
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
	/* Let's try to inject the equivalent of:
void t(int *__restrict__ a, int *__restrict__ b, char *__restrict__ c) {
	*a += *c;
	*b += *c;
}
	*/
	gcc_jit_type *int_type =
		gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
	gcc_jit_type *pint_type = gcc_jit_type_get_pointer(int_type);
	gcc_jit_type *pint_restrict_type = gcc_jit_type_get_restrict(pint_type);

	gcc_jit_type *void_type =
		gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);

	gcc_jit_param *a =
		gcc_jit_context_new_param (ctxt, NULL, pint_restrict_type, "a");
	gcc_jit_param *b =
		gcc_jit_context_new_param (ctxt, NULL, pint_restrict_type, "b");
	gcc_jit_param *c =
		gcc_jit_context_new_param (ctxt, NULL, pint_restrict_type, "c");
	gcc_jit_param *params[3] = {a, b, c};

	gcc_jit_function *func_t =
		gcc_jit_context_new_function (ctxt, NULL,
					GCC_JIT_FUNCTION_EXPORTED,
					void_type,
					"t",
					3, params,
					0);

	gcc_jit_block *block = gcc_jit_function_new_block (func_t, NULL);

	/* *a += *c; */
	gcc_jit_block_add_assignment_op (
		block, NULL,
		gcc_jit_rvalue_dereference (gcc_jit_param_as_rvalue (a), NULL),
		GCC_JIT_BINARY_OP_PLUS,
		gcc_jit_lvalue_as_rvalue (
			gcc_jit_rvalue_dereference (gcc_jit_param_as_rvalue (c), NULL)));
	/* *b += *c; */
	gcc_jit_block_add_assignment_op (
		block, NULL,
		gcc_jit_rvalue_dereference (gcc_jit_param_as_rvalue (b), NULL),
		GCC_JIT_BINARY_OP_PLUS,
		gcc_jit_lvalue_as_rvalue (
			gcc_jit_rvalue_dereference (gcc_jit_param_as_rvalue (c), NULL)));

	gcc_jit_block_end_with_void_return (block, NULL);
}

/* { dg-final { jit-verify-output-file-was-created "" } } */
/* { dg-final { jit-verify-assembler-output "addl	%eax, (%rdi)
	addl	%eax, (%rsi)" } } */
