/* Test for PR 69625; make sure that a leaf vararg function does not overwrite
   the caller's r6.  */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

__attribute__ ((noinline))
int
foo (int x, ...)
{
  __builtin_va_list vl;
  int i;

  __asm__ __volatile__ ("lhi %%r6,1" : : : "r6");
  __builtin_va_start(vl, x);
  for (i = 2; i <= 6; i++)
    x += __builtin_va_arg(vl, int);
  __builtin_va_end (vl);

  return x;
}

__attribute__ ((noinline))
void
bar (int r2, int r3, int r4, int r5, int r6)
{
  foo (r2, r3, r4, r5, r6);
  if (r6 != 6)
    abort ();
}

int
main (void)
{
  bar (2, 3, 4, 5, 6);
}
