/* { dg-do compile } */
/* { dg-options "-O2 -mminimal-toc -mno-multiple" } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */

void foo ()
{
  long l; void *p;
  volatile int x;

  __builtin_unwind_init ();
  x = 12;
  __builtin_eh_return (l, p);
}

/* { dg-final { scan-assembler "st\[wd\] 30," } } */
