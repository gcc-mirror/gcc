/* { dg-do run } */
/* { dg-options "-O2 -fno-inline" } */

/* With BTI enabled, this test would crash with SIGILL, Illegal instruction.
   The 2nd argument of __builtin_eh_return is expected to be an EH handler
   within a function, rather than a separate function.
   The current implementation of __builtin_eh_return in AArch64 backend emits a
   jump instead of branching with LR.
   The prologue of the handler (i.e. continuation) starts with "bti c" (vs.
   "bti jc") which is a landing pad type prohibiting jumps, hence the exception
   at runtime.
   The current behavior of __builtin_eh_return is considered correct.
   Consequently, the default option -mbranch-protection=standard needs to be
   overridden to remove BTI.  */
/* { dg-additional-options "-mbranch-protection=pac-ret+leaf+gcs" { target { default_branch_protection } } } */

#include <stdlib.h>
#include <stdio.h>

int val, test, failed;

int main (void);

void
eh0 (void *p)
{
  val = (int)(long)p & 7;
  if (val)
    abort ();
}

void
eh1 (void *p, int x)
{
  void *q = __builtin_alloca (x);
  eh0 (q);
  __builtin_eh_return (0, p);
}

void
eh2a (int a,int b,int c,int d,int e,int f,int g,int h, void *p)
{
  val = a + b + c + d + e + f + g + h +  (int)(long)p & 7;
}

void
eh2 (void *p)
{
  eh2a (val, val, val, val, val, val, val, val, p);
  __builtin_eh_return (0, p);
}


void
continuation (void)
{
  test++;
  main ();
}

void
fail (void)
{
  failed = 1;
  printf ("failed\n");
  continuation ();
}

void
do_test1 (void)
{
  if (!val)
    eh1 (continuation, 100);
  fail ();
}

void
do_test2 (void)
{
  if (!val)
    eh2 (continuation);
  fail ();
}

int
main (void)
{
  if (test == 0)
    do_test1 ();
  if (test == 1)
    do_test2 ();
  if (failed || test != 2)
    exit (1);
  exit (0);
}
