/* This code is shared by various test-compile-to-*.c test cases
   that ultimately generate a standalone executable
   (all of them apart from test-compile-to-dynamic-library.c).  */

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:
     static void
     hello_world (const char *name)
     {
       // a test comment
       printf ("hello from %s\n", name);
     }

     extern int
     main (int argc, char **argv)
     {
       hello_world (argv[0]);
       return 0;
     }
  */
  gcc_jit_type *void_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_type *const_char_ptr_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CONST_CHAR_PTR);
  gcc_jit_param *param_name =
    gcc_jit_context_new_param (ctxt, NULL, const_char_ptr_type, "name");
  gcc_jit_function *func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_INTERNAL,
				  void_type,
				  "hello_world",
				  1, &param_name,
				  0);

  gcc_jit_param *param_format =
    gcc_jit_context_new_param (ctxt, NULL, const_char_ptr_type, "format");
  gcc_jit_function *printf_func =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_IMPORTED,
				  gcc_jit_context_get_type (
				     ctxt, GCC_JIT_TYPE_INT),
				  "printf",
				  1, &param_format,
				  1);
  gcc_jit_rvalue *args[2];
  args[0] = gcc_jit_context_new_string_literal (ctxt, "hello from %s\n");
  args[1] = gcc_jit_param_as_rvalue (param_name);

  gcc_jit_block *block = gcc_jit_function_new_block (func, NULL);

  gcc_jit_block_add_comment (
    block, NULL,
    "a test comment");

  gcc_jit_block_add_eval (
    block, NULL,
    gcc_jit_context_new_call (ctxt,
			      NULL,
			      printf_func,
			      2, args));
  gcc_jit_block_end_with_void_return (block, NULL);

  gcc_jit_type *int_type = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_param *param_argc =
    gcc_jit_context_new_param (ctxt, NULL, int_type, "argc");
  gcc_jit_type *char_ptr_ptr_type =
    gcc_jit_type_get_pointer (
      gcc_jit_type_get_pointer (
	gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_CHAR)));
  gcc_jit_param *param_argv =
    gcc_jit_context_new_param (ctxt, NULL, char_ptr_ptr_type, "argv");
  gcc_jit_param *params[2] = {param_argc, param_argv};
  gcc_jit_function *func_main =
    gcc_jit_context_new_function (ctxt, NULL,
				  GCC_JIT_FUNCTION_EXPORTED,
				  int_type,
				  "main",
				  2, params,
				  0);
  block = gcc_jit_function_new_block (func_main, NULL);
  gcc_jit_rvalue *zero = gcc_jit_context_zero (ctxt, int_type);
  args[0] = gcc_jit_context_new_cast (
	ctxt,
	NULL,
	gcc_jit_lvalue_as_rvalue (
	  gcc_jit_context_new_array_access (
	    ctxt,
	    NULL,
	    gcc_jit_param_as_rvalue (param_argv),
	    zero)),
	const_char_ptr_type);
  gcc_jit_block_add_eval (
    block, NULL,
    gcc_jit_context_new_call (ctxt,
			      NULL,
			      func,
			      1, args));
  gcc_jit_block_end_with_return (block, NULL, zero);
}
