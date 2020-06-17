#include "test-setjmp.h"
#include <stddef.h>
#include <stdlib.h>

extern int foo (int) __attribute__ ((__pure__));

static jmp_buf env;

static void inner (void)
{
  void *ptr = malloc (1024); /* { dg-message "allocated here" }  */
  longjmp (env, 1); /* { dg-warning "leak of 'ptr'" } */
  free (ptr);
}

void outer (void)
{
  int i;

  foo (0);

  i = SETJMP(env);

  if (i == 0)
    {
      foo (1);
      inner ();
    }

  foo (3);
}
