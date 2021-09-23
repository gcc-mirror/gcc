/* { dg-do compile { target x86_64-*-* } } */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include "libgccjit.h"

#include "harness.h"

/**********************************************************************
 Support fns for creating code.
 **********************************************************************/

/* Make a "void FUNC_NAME (void)" function with a single block, returning
   that block.  */

static gcc_jit_block *
make_single_block_func (gcc_jit_context *ctxt, const char *func_name)
{
  gcc_jit_type *void_type = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_VOID);
  gcc_jit_function *func
    = gcc_jit_context_new_function (ctxt, NULL,
				    GCC_JIT_FUNCTION_EXPORTED,
				    void_type,
				    func_name,
				    0, NULL, 0);
  return gcc_jit_function_new_block (func, "initial");
}

static const char *
get_desc (gcc_jit_extended_asm *ext_asm)
{
  return gcc_jit_object_get_debug_string
    (gcc_jit_extended_asm_as_object (ext_asm));
}

/**********************************************************************
 Support fns for verifying code.
 **********************************************************************/

typedef void (*void_void_fn) (void);

static void_void_fn
get_test_fn (gcc_jit_result *result, const char *func_name)
{
  return (void_void_fn)gcc_jit_result_get_code (result, func_name);
}

/**********************************************************************
 test_i386_basic_asm_1: simple example of asm
 **********************************************************************/

/* Create the equivalent of:

     int src;
     int dst;

     void test_i386_basic_asm_1 (void)
     {
       // Quote from here in docs/topics/asm.rst: example 1: C
       asm ("mov %1, %0\n\t"
            "add $1, %0"
            : "=r" (dst)
            : "r" (src));
       // Quote up to here in docs/topics/asm.rst: example 1: C
     }

     i.e. copy src to dst and add 1 to dst.  */

static void
create_test_i386_basic_asm_1 (gcc_jit_context *ctxt)
{
  gcc_jit_type *int_type = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_lvalue *dst
    = gcc_jit_context_new_global (ctxt, NULL,
				  GCC_JIT_GLOBAL_EXPORTED,
				  int_type, "dst");
  gcc_jit_lvalue *src
    = gcc_jit_context_new_global (ctxt, NULL,
				  GCC_JIT_GLOBAL_EXPORTED,
				  int_type, "src");

  gcc_jit_block *block
    = make_single_block_func (ctxt, "test_i386_basic_asm_1");

  /* Quote from here in docs/topics/asm.rst: example 1: jit.  */
  gcc_jit_extended_asm *ext_asm
    = gcc_jit_block_add_extended_asm (block, NULL,
				      "mov %1, %0\n\t"
				      "add $1, %0");
  gcc_jit_extended_asm_add_output_operand (ext_asm, NULL, "=r", dst);
  gcc_jit_extended_asm_add_input_operand (ext_asm, NULL, "r",
					  gcc_jit_lvalue_as_rvalue (src));
  /* Quote up to here in docs/topics/asm.rst: example 1: jit.  */

  const char *desc = get_desc (ext_asm);
  CHECK_STRING_VALUE
    (desc,
     "asm (\"mov %1, %0\\n\\tadd $1, %0\" : \"=r\" (dst) : \"r\" (src) : )");

  gcc_jit_block_end_with_void_return (block, NULL);
}

static void
verify_code_1 (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  void_void_fn test_i386_basic_asm_1
    = get_test_fn (result, "test_i386_basic_asm_1");
  CHECK_NON_NULL (test_i386_basic_asm_1);

  int *dst_ptr = (int *)gcc_jit_result_get_global (result, "dst");
  CHECK_NON_NULL (dst_ptr);
  int *src_ptr = (int *)gcc_jit_result_get_global (result, "src");
  CHECK_NON_NULL (src_ptr);

  *src_ptr = 42;
  *dst_ptr = 0;
  test_i386_basic_asm_1 ();
  CHECK_VALUE (*src_ptr, 42);
  CHECK_VALUE (*dst_ptr, 43);
}

/**********************************************************************
 test_i386_basic_asm_2: test of symbolic names and clobbers
 **********************************************************************/

/* Create the equivalent of:
     uint32_t test_i386_basic_asm_2 (uint32_t Mask)
     {
       uint32_t Index;
       // Quote from here in docs/topics/asm.rst: example 2: C
       asm ("bsfl %[aMask], %[aIndex]"
            : [aIndex] "=r" (Index)
            : [aMask] "r" (Mask)
            : "cc");
       // Quote up to here in docs/topics/asm.rst: example 2: C
       return Index;
     }
   i.e. return the first bit set in "Mask"

   This exercises symbolic names and clobbers.  */

static void
create_test_i386_basic_asm_2 (gcc_jit_context *ctxt)
{
  gcc_jit_type *uint32_type = gcc_jit_context_get_int_type (ctxt, 4, 0);
  gcc_jit_param *mask
    = gcc_jit_context_new_param (ctxt, NULL,
				 uint32_type, "Mask");
  gcc_jit_function *func
    = gcc_jit_context_new_function (ctxt, NULL,
				    GCC_JIT_FUNCTION_EXPORTED,
				    uint32_type,
				    "test_i386_basic_asm_2",
				    1, &mask, 0);
  gcc_jit_lvalue *index
    = gcc_jit_function_new_local (func, NULL,
				  uint32_type, "Index");
  gcc_jit_block *block = gcc_jit_function_new_block (func, "initial");

  /* Quote from here in docs/topics/asm.rst: example 2: jit.  */
  gcc_jit_extended_asm *ext_asm
    = gcc_jit_block_add_extended_asm (block, NULL,
				      "bsfl %[aMask], %[aIndex]");
  gcc_jit_extended_asm_add_output_operand (ext_asm, "aIndex", "=r", index);
  gcc_jit_extended_asm_add_input_operand (ext_asm, "aMask", "r",
					  gcc_jit_param_as_rvalue (mask));
  gcc_jit_extended_asm_add_clobber (ext_asm, "cc");
  /* Quote up to here in docs/topics/asm.rst: example 2: jit.  */

  const char *desc = get_desc (ext_asm);
  CHECK_STRING_VALUE
    (desc,
     "asm (\"bsfl %[aMask], %[aIndex]\""
     " : [aIndex] \"=r\" (Index) : [aMask] \"r\" (Mask) : \"cc\")");

  gcc_jit_block_end_with_return (block, NULL,
				 gcc_jit_lvalue_as_rvalue (index));
}

static void
verify_code_2 (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef uint32_t (*fntype) (uint32_t);
  fntype test_i386_basic_asm_2
    = (fntype)gcc_jit_result_get_code (result, "test_i386_basic_asm_2");
  CHECK_NON_NULL (test_i386_basic_asm_2);

  CHECK_VALUE (test_i386_basic_asm_2 (1), 0);
  CHECK_VALUE (test_i386_basic_asm_2 (2), 1);
  CHECK_VALUE (test_i386_basic_asm_2 (4), 2);
  CHECK_VALUE (test_i386_basic_asm_2 (8), 3);
}

/**********************************************************************
 test_i386_basic_asm_3a/b: test of control flow: "asm goto"
 **********************************************************************/

/* Create the equivalent of:

     int test_i386_basic_asm_3a (int p1, int p2)
     {
       asm goto ("btl %1, %0\n\t"
		 "jc %l2"
		 : // No outputs
		 : "r" (p1), "r" (p2)
		 : "cc"
		 : carry);

       return 0;

      carry:
       return 1;
     }

    or (the "_3b" variant) using a name rather than a number for the goto
    label:

       // Quote from here in docs/topics/asm.rst: example 3b: C
       asm goto ("btl %1, %0\n\t"
                 "jc %l[carry]"
                 : // No outputs
                 : "r" (p1), "r" (p2)
                 : "cc"
                 : carry);
       // Quote up to here in docs/topics/asm.rst: example 3b: C

    This exercises control flow with an asm.  */

static void
create_test_i386_basic_asm_3 (gcc_jit_context *ctxt,
			      const char *funcname,
			      int use_name)
{
  gcc_jit_type *int_type = gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_INT);
  gcc_jit_param *p1 = gcc_jit_context_new_param (ctxt, NULL, int_type, "p1");
  gcc_jit_param *p2 = gcc_jit_context_new_param (ctxt, NULL, int_type, "p2");
  gcc_jit_param *params[2] = {p1, p2};
  gcc_jit_function *func
    = gcc_jit_context_new_function (ctxt, NULL,
				    GCC_JIT_FUNCTION_EXPORTED,
				    int_type,
				    funcname,
				    2, params, 0);
  gcc_jit_block *b_start = gcc_jit_function_new_block (func, "start");
  gcc_jit_block *b_fallthru = gcc_jit_function_new_block (func, "fallthru");
  gcc_jit_block *b_carry = gcc_jit_function_new_block (func, "carry");

  gcc_jit_rvalue *zero
    = gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 0);
  gcc_jit_rvalue *one
    = gcc_jit_context_new_rvalue_from_int (ctxt, int_type, 1);

  /* Quote from here in docs/topics/asm.rst: example 3: jit.  */
  const char *asm_template =
    (use_name
     ? /* Label referred to by name: "%l[carry]".  */
       ("btl %1, %0\n\t"
        "jc %l[carry]")
     : /* Label referred to numerically: "%l2".  */
       ("btl %1, %0\n\t"
        "jc %l2"));

  gcc_jit_extended_asm *ext_asm
    = gcc_jit_block_end_with_extended_asm_goto (b_start, NULL,
						asm_template,
						1, &b_carry,
						b_fallthru);
  gcc_jit_extended_asm_add_input_operand (ext_asm, NULL, "r",
					  gcc_jit_param_as_rvalue (p1));
  gcc_jit_extended_asm_add_input_operand (ext_asm, NULL, "r",
					  gcc_jit_param_as_rvalue (p2));
  gcc_jit_extended_asm_add_clobber (ext_asm, "cc");
  /* Quote up to here in docs/topics/asm.rst: example 3: jit.  */

  const char *desc = get_desc (ext_asm);
  CHECK_STRING_VALUE
    (desc,
     (use_name
      ? ("asm goto (\"btl %1, %0\\n\\tjc %l[carry]\" "
	 ":  : \"r\" (p1), \"r\" (p2) : \"cc\" "
	 ": carry [fallthrough: fallthru])")
      : ("asm goto (\"btl %1, %0\\n\\tjc %l2\" "
	 ":  : \"r\" (p1), \"r\" (p2) : \"cc\" "
	 ": carry [fallthrough: fallthru])")));

  gcc_jit_block_end_with_return (b_fallthru, NULL, zero);
  gcc_jit_block_end_with_return (b_carry, NULL, one);
}

static void
verify_code_3 (gcc_jit_context *ctxt, gcc_jit_result *result,
	       const char *funcname)
{
  typedef int (*test_i386_basic_asm_3_type) (int, int);

  test_i386_basic_asm_3_type test_i386_basic_asm_3
    = (test_i386_basic_asm_3_type) gcc_jit_result_get_code (result, funcname);
  CHECK_NON_NULL (test_i386_basic_asm_3);

  /* The fn should test bits, returning 0 or 1.  */
  /* Bit 0.  */
  CHECK_VALUE (test_i386_basic_asm_3 (0x0000, 0), 0);
  CHECK_VALUE (test_i386_basic_asm_3 (0x0001, 0), 1);
  CHECK_VALUE (test_i386_basic_asm_3 (0x0002, 0), 0);
  CHECK_VALUE (test_i386_basic_asm_3 (0x0003, 0), 1);
  CHECK_VALUE (test_i386_basic_asm_3 (0x0004, 0), 0);
  /* Bit 1.  */
  CHECK_VALUE (test_i386_basic_asm_3 (0x0000, 1), 0);
  CHECK_VALUE (test_i386_basic_asm_3 (0x0001, 1), 0);
  CHECK_VALUE (test_i386_basic_asm_3 (0x0002, 1), 1);
  CHECK_VALUE (test_i386_basic_asm_3 (0x0003, 1), 1);
  CHECK_VALUE (test_i386_basic_asm_3 (0x0004, 1), 0);

  for (int i = 0; i < 15; i++)
    {
      CHECK_VALUE (test_i386_basic_asm_3 (0x0000, i), 0);
      CHECK_VALUE (test_i386_basic_asm_3 (0xffff, i), 1);
    }
}

/**********************************************************************
 test_i386_basic_asm_4: test of "volatile"
 **********************************************************************/

/* Create the equivalent of:
     uint64_t test_i386_basic_asm_4 (void)
     {
       uint64_t start_time, end_time;

       // Get start time
       asm volatile ("rdtsc\n\t"    // Returns the time in EDX:EAX.
                     "shl $32, %%rdx\n\t"  // Shift the upper bits left.
                     "or %%rdx, %0"        // 'Or' in the lower bits.
                     : "=a" (start_time)
                     :
                     : "rdx");

       // could do other work here

       // Get end time
       asm volatile ("rdtsc\n\t"    // Returns the time in EDX:EAX.
                     "shl $32, %%rdx\n\t"  // Shift the upper bits left.
                     "or %%rdx, %0"        // 'Or' in the lower bits.
                     : "=a" (start_time)
                     :
                     : "rdx");

       // Get elapsed time
       return end_time - start_time;
     }

   This exercises "volatile"; without it, the optimizer can assume that
   both asm generate the same value and thus the time difference is zero.  */

static void
add_rdtsc (gcc_jit_block *block, gcc_jit_lvalue *msr)
{
  /* Quote from here in docs/topics/asm.rst: example 4: jit.  */
  gcc_jit_extended_asm *ext_asm
    = gcc_jit_block_add_extended_asm
	(block, NULL,
	 "rdtsc\n\t"  /* Returns the time in EDX:EAX.  */
	 "shl $32, %%rdx\n\t"  /* Shift the upper bits left.  */
	 "or %%rdx, %0");  /* 'Or' in the lower bits.  */
  gcc_jit_extended_asm_set_volatile_flag (ext_asm, 1);
  gcc_jit_extended_asm_add_output_operand (ext_asm, NULL, "=a", msr);
  gcc_jit_extended_asm_add_clobber (ext_asm, "rdx");
  /* Quote up to here in docs/topics/asm.rst: example 4: jit.  */

  const char *desc = get_desc (ext_asm);
  CHECK_STRING_STARTS_WITH (desc, "asm volatile (");
}

static void
create_test_i386_basic_asm_4 (gcc_jit_context *ctxt)
{
  gcc_jit_type *uint64_type = gcc_jit_context_get_int_type (ctxt, 8, 0);
  gcc_jit_function *func
    = gcc_jit_context_new_function (ctxt, NULL,
				    GCC_JIT_FUNCTION_EXPORTED,
				    uint64_type,
				    "test_i386_basic_asm_4",
				    0, NULL, 0);
  gcc_jit_block *block = gcc_jit_function_new_block (func, NULL);

  gcc_jit_lvalue *start_time
    = gcc_jit_function_new_local (func, NULL, uint64_type, "start_time");
  add_rdtsc (block, start_time);

  gcc_jit_block_add_comment (block, NULL, "other work here");

  gcc_jit_lvalue *end_time
    = gcc_jit_function_new_local (func, NULL, uint64_type, "end_time");
  add_rdtsc (block, end_time);

  gcc_jit_rvalue *elapsed
    = gcc_jit_context_new_binary_op (ctxt, NULL, GCC_JIT_BINARY_OP_MINUS,
				     uint64_type,
				     gcc_jit_lvalue_as_rvalue (end_time),
				     gcc_jit_lvalue_as_rvalue (start_time));
  gcc_jit_block_end_with_return (block, NULL, elapsed);
}

static void
verify_code_4 (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef uint64_t (*fntype) (void);
  fntype test_i386_basic_asm_4
    = (fntype)gcc_jit_result_get_code (result, "test_i386_basic_asm_4");

  CHECK_NON_NULL (test_i386_basic_asm_4);

  test_i386_basic_asm_4 ();
}

/**********************************************************************
 test_i386_basic_asm_5: test of top-level asm
 **********************************************************************/

/* Create the equivalent of:

   // Quote from here in docs/topics/asm.rst: example 5: C
     asm ("\t.pushsection .text\n"
          "\t.globl add_asm\n"
          "\t.type add_asm, @function\n"
          "add_asm:\n"
          "\tmovq %rdi, %rax\n"
          "\tadd %rsi, %rax\n"
          "\tret\n"
          "\t.popsection\n");
   // Quote up to here in docs/topics/asm.rst: example 5: C

   to add a simple function ("add_asm") directly in assembly language.  */

static void
create_test_i386_basic_asm_5 (gcc_jit_context *ctxt)
{
#if __APPLE__
  /* Darwin's assemblers do not support push/pop section, do not use .type
     and external symbols should use __USER_LABEL_PREFIX__.  */
  gcc_jit_context_add_top_level_asm (ctxt, NULL,
                                     "\t.text\n"
                                     "\t.globl _add_asm\n"
                                     "_add_asm:\n"
                                     "\tmovq %rdi, %rax\n"
                                     "\tadd %rsi, %rax\n"
                                     "\tret\n"
                                     "\t# some asm here\n");
#else
  /* Quote from here in docs/topics/asm.rst: example 5: jit.  */
  gcc_jit_context_add_top_level_asm (ctxt, NULL,
                                     "\t.pushsection .text\n"
                                     "\t.globl add_asm\n"
                                     "\t.type add_asm, @function\n"
                                     "add_asm:\n"
                                     "\tmovq %rdi, %rax\n"
                                     "\tadd %rsi, %rax\n"
                                     "\tret\n"
                                     "\t# some asm here\n"
                                     "\t.popsection\n");
  /* Quote up to here in docs/topics/asm.rst: example 5: jit.  */
#endif
}

static void
verify_code_5 (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  typedef int (*test_i386_basic_asm_5_type) (int, int);
  test_i386_basic_asm_5_type test_i386_basic_asm_5
    = (test_i386_basic_asm_5_type) gcc_jit_result_get_code (result, "add_asm");
  CHECK_NON_NULL (test_i386_basic_asm_5);

  CHECK_VALUE (test_i386_basic_asm_5 (2, 2), 4);
  CHECK_VALUE (test_i386_basic_asm_5 (20, 7), 27);
}

/**********************************************************************
 Code for harness
 **********************************************************************/

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  create_test_i386_basic_asm_1 (ctxt);
  create_test_i386_basic_asm_2 (ctxt);
  create_test_i386_basic_asm_3 (ctxt, "test_i386_basic_asm_3a", 0);
  create_test_i386_basic_asm_3 (ctxt, "test_i386_basic_asm_3b", 1);
  create_test_i386_basic_asm_4 (ctxt);
  create_test_i386_basic_asm_5 (ctxt);
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);
  verify_code_1 (ctxt, result);
  verify_code_2 (ctxt, result);
  verify_code_3 (ctxt, result, "test_i386_basic_asm_3a");
  verify_code_3 (ctxt, result, "test_i386_basic_asm_3b");
  verify_code_4 (ctxt, result);
  verify_code_5 (ctxt, result);
}
