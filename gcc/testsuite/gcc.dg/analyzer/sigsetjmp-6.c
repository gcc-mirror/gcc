/* { dg-require-effective-target sigsetjmp } */

#include <setjmp.h>
#include <stddef.h>
#include <stdlib.h>

extern int foo (int) __attribute__ ((__pure__));

static jmp_buf env;

static void inner (void)
{
  void *ptr = malloc (1024); /* { dg-message "allocated here" }  */

  siglongjmp (env, 1); /* { dg-warning "leak of 'ptr'" "warning" } */
  /* { dg-message "rewinding from 'siglongjmp' in 'inner'" " event: rewind from" { target *-*-* } .-1 } */

  free (ptr);
}

void outer (void)
{
  int i;

  foo (0);

  i = sigsetjmp(env, 0); /* { dg-message "'sigsetjmp' called here" "event: sigsetjmp call" } */
  /* { dg-message "to 'sigsetjmp' in 'outer'" "event: rewind to"  { target *-*-* } .-1 } */

  if (i == 0)
    {
      foo (1);
      inner ();
    }

  foo (3);
}
