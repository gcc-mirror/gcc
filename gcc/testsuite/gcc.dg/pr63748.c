/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */
/* { dg-require-effective-target nonlocal_goto } */

#include <setjmp.h>

jmp_buf *alloc_jmp_buf ();
int foo (void *);

int
test (int op, int noside)
{
  void *argvec = 0;

  if (op)
    {
      jmp_buf *buf = alloc_jmp_buf (); /* { dg-bogus "uninitialized" } */
      setjmp (*buf);

      if (noside)
        goto nosideret;

    do_call_it:

      if (noside)
        goto nosideret;

      return foo (argvec);
    }

  argvec = __builtin_alloca (1);
  goto do_call_it;

nosideret:
  return 1;
}

