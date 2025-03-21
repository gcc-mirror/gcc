/* { dg-do compile { target x86_64-*-* } } */

#include <stdlib.h>
#include <stdio.h>

#include "libgccjit.h"

#define TEST_PROVIDES_MAIN
#include "harness.h"

void
create_code (gcc_jit_context *ctxt, void *user_data)
{
}

void
verify_code (gcc_jit_context *ctxt, gcc_jit_result *result)
{
}

int
main (int argc, char **argv)
{
  /*  This is the same as the main provided by harness.h, but calls gcc_jit_context_get_target_info.  */
  gcc_jit_context *ctxt;
  ctxt = gcc_jit_context_acquire ();
  if (!ctxt)
    {
      fail ("gcc_jit_context_acquire failed");
      return -1;
    }
  gcc_jit_target_info* info = gcc_jit_context_get_target_info (ctxt);
  gcc_jit_context_compile (ctxt);

  gcc_jit_target_info_release (info);

  /* Verify that the correct error message was emitted.  */
  CHECK_STRING_VALUE (gcc_jit_context_get_last_error (ctxt),
    "cannot compile after calling gcc_jit_context_get_target_info");

  gcc_jit_context_release (ctxt);

  int i;

  for (i = 1; i <= 5; i++)
    {
      snprintf (test, sizeof (test),
		"%s iteration %d of %d",
                extract_progname (argv[0]),
                i, 5);

      //printf ("ITERATION %d\n", i);
      test_jit (argv[0], NULL);
      //printf ("\n");
    }

  totals ();

  return 0;
}
