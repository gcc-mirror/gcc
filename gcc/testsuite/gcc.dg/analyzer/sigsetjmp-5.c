/* { dg-require-effective-target sigsetjmp } */

#include "test-setjmp.h"
#include <stddef.h>
#include "analyzer-decls.h"

static sigjmp_buf env;

static void inner (void)
{
  sigsetjmp (env, 0); /* { dg-message "'sigsetjmp' called here" } */
}

void outer (void)
{
  int i;

  inner ();

  siglongjmp (env, 42); /* { dg-warning "'siglongjmp' called after enclosing function of 'sigsetjmp' has returned" } */
}
