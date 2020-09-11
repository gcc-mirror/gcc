#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "libgccjit.h"

#include "harness.h"

#define BIG_BLOB_SIZE (1 << 12) /* 4KB.  */

static signed char test_blob1[] = { 0xc, 0xa, 0xf, 0xf, 0xe };
static unsigned test_blob2[] = { 0x3, 0x2, 0x1, 0x0, 0x1, 0x2, 0x3 };
static unsigned char test_blob3[BIG_BLOB_SIZE];

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
  /* Let's try to inject the equivalent of:

     signed char bin_blob1[] = { 0xc, 0xa, 0xf, 0xf, 0xe };
     unsigned bin_blob2[] = { 0x3, 0x2, 0x1, 0x0, 0x1, 0x2, 0x3 };
     unsigned char bin_blob3[4096]...
  */
  gcc_jit_type *unsigned_char_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UNSIGNED_CHAR);
  gcc_jit_type *signed_char_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_SIGNED_CHAR);
  gcc_jit_type *unsigned_type =
    gcc_jit_context_get_type (ctxt, GCC_JIT_TYPE_UNSIGNED_INT);

  gcc_jit_lvalue *glob =
    gcc_jit_context_new_global (
      ctxt, NULL, GCC_JIT_GLOBAL_EXPORTED,
      gcc_jit_context_new_array_type (ctxt, NULL, signed_char_type,
				      sizeof (test_blob1)),
      "bin_blob1");
  gcc_jit_global_set_initializer (glob, test_blob1, sizeof (test_blob1));

  glob =
    gcc_jit_context_new_global (
      ctxt, NULL, GCC_JIT_GLOBAL_EXPORTED,
      gcc_jit_context_new_array_type (
	ctxt, NULL, unsigned_type,
	sizeof (test_blob2) / sizeof (*test_blob2)),
      "bin_blob2");
  gcc_jit_global_set_initializer (glob, test_blob2,
				  sizeof (test_blob2));

  for (size_t i = 0; i < BIG_BLOB_SIZE; i++)
    test_blob3[i] = i * i + i;
  glob =
    gcc_jit_context_new_global (
      ctxt, NULL, GCC_JIT_GLOBAL_EXPORTED,
      gcc_jit_context_new_array_type (ctxt, NULL, unsigned_char_type,
				      sizeof (test_blob3)),
      "bin_blob3");
  gcc_jit_global_set_initializer (glob, test_blob3, sizeof (test_blob3));
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
  CHECK_NON_NULL (result);
  void *glob = gcc_jit_result_get_global (result, "bin_blob1");
  CHECK_NON_NULL (glob);
  CHECK_VALUE (memcmp (test_blob1, glob, sizeof (test_blob1)), 0);

  glob = gcc_jit_result_get_global (result, "bin_blob2");
  CHECK_NON_NULL (glob);
  CHECK_VALUE (memcmp (test_blob2, glob,
		       sizeof (test_blob2)), 0);

  glob = gcc_jit_result_get_global (result, "bin_blob3");
  CHECK_NON_NULL (glob);
  CHECK_VALUE (memcmp (test_blob3, glob, sizeof (test_blob3)), 0);

}
