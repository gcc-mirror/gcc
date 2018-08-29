/* { dg-do compile } */
/* { dg-options "-O2 -funroll-all-loops -fdump-rtl-loop2_unroll-details -fdump-tree-cunrolli-details" } */

extern void bar (int);

int j;

void test (void)
{
  #pragma GCC unroll 0
  #pragma GCC ivdep
  for (unsigned long i = 1; i <= 3; ++i)
    bar(i);

  #pragma GCC ivdep
  #pragma GCC unroll 0
  for (unsigned long i = 1; i <= j; ++i)
    bar(i);

  /* { dg-final { scan-tree-dump "Not unrolling loop .: user didn't want it unrolled completely" "cunrolli" } } */
  /* { dg-final { scan-rtl-dump-times "Not unrolling loop, user didn't want it unrolled" 2 "loop2_unroll" } } */
}
