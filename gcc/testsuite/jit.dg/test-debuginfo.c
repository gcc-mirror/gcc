/* Essentially this test checks that debug info are generated for globals
   locals and functions, including type info. The comment bellow is used
   as fake code (does not affect the test, use for manual debugging). */
/*
int a_global_for_test_debuginfo;
int main (int argc, char **argv)
{
    int a_local_for_test_debuginfo = 2;
    return a_global_for_test_debuginfo + a_local_for_test_debuginfo;
}
*/
#include "libgccjit.h"

/* We don't want set_options() in harness.h to set -O3 so our little local
   is optimized away. */
#define TEST_ESCHEWS_SET_OPTIONS
static void set_options (gcc_jit_context *ctxt, const char *argv0)
{
    gcc_jit_context_set_bool_option(ctxt, GCC_JIT_BOOL_OPTION_DEBUGINFO, 1);
}

#define TEST_COMPILING_TO_FILE
#define OUTPUT_KIND      GCC_JIT_OUTPUT_KIND_EXECUTABLE
#define OUTPUT_FILENAME  "jit-debuginfo.o"
#include "harness.h"

#define LOC(row, col) gcc_jit_context_new_location(ctxt, "test-debuginfo.c", row, col)

void
create_code (gcc_jit_context *ctxt, void* p)
{
  gcc_jit_type *int_type = gcc_jit_context_get_type(ctxt, GCC_JIT_TYPE_INT);

  gcc_jit_lvalue *bar = gcc_jit_context_new_global(ctxt, 
    LOC(5,1), GCC_JIT_GLOBAL_EXPORTED, 
    int_type, "a_global_for_test_debuginfo");

  gcc_jit_param *argc_para = gcc_jit_context_new_param(ctxt, LOC(6,15), 
    int_type, "argc");
  gcc_jit_param *argv_para = gcc_jit_context_new_param(ctxt, LOC(6,28), 
    gcc_jit_type_get_pointer(
      gcc_jit_type_get_pointer(
        gcc_jit_context_get_type(ctxt, GCC_JIT_TYPE_CHAR))),
    "argc");

  gcc_jit_param *params[] = {argc_para, argv_para};

  gcc_jit_function *foo_fn = gcc_jit_context_new_function(ctxt, LOC(6,5), 
    GCC_JIT_FUNCTION_EXPORTED, int_type, "main", 2, params, 0);
  gcc_jit_block *start_block = gcc_jit_function_new_block(foo_fn, 
    "start_block");

  gcc_jit_lvalue *a = gcc_jit_function_new_local(foo_fn, LOC(8,5), 
    int_type, "a_local_for_test_debuginfo");
  gcc_jit_block_add_assignment(start_block, LOC(8,36), a, 
    gcc_jit_context_new_rvalue_from_int(ctxt, int_type, 2));
  gcc_jit_rvalue *add = gcc_jit_context_new_binary_op(ctxt, LOC(9,40), 
    GCC_JIT_BINARY_OP_PLUS, int_type, 
    gcc_jit_lvalue_as_rvalue(a), gcc_jit_lvalue_as_rvalue(bar));

  gcc_jit_block_end_with_return(start_block, LOC(9,5), add);
}

#undef LOC

/* jit-check-debug-info fires up gdb and checks that the variables have 
   debug info */

/*  { dg-final { jit-check-debug-info "jit-debuginfo.o" {"info variables\n"} "int\\s+a_global_for_test_debuginfo;" } } */
/*  { dg-final { jit-check-debug-info "jit-debuginfo.o" {"pt main\n"} "int\\s*\\(\\s*int\\s*,\\s*char\\s*\\*\\*\\s*\\)"} } */
/*  { dg-final { jit-check-debug-info "jit-debuginfo.o" {"start\n" "info locals\n"} "a_local_for_test_debuginfo"} } */
/*  { dg-final { jit-check-debug-info "jit-debuginfo.o" {"start\n" "pt a_local_for_test_debuginfo\n"} "int"} } */