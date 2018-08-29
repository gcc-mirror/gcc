/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cunrolli-details -fdump-rtl-loop2_unroll-details" } */

extern void bar (int);

int j;

void test (void)
{
  #pragma GCC unroll 8
  for (unsigned long i = 1; i <= 8; ++i)
    bar(i);
  /* { dg-final { scan-tree-dump "11:.*: loop with 8 iterations completely unrolled" "cunrolli" } } */

  #pragma GCC unroll 8
  for (unsigned long i = 1; i <= 7; ++i)
    bar(i);
  /* { dg-final { scan-tree-dump "16:.*: loop with 7 iterations completely unrolled" "cunrolli" } } */

  #pragma GCC unroll 8
  for (unsigned long i = 1; i <= 15; ++i)
    bar(i);
  /* { dg-final { scan-rtl-dump "21:.*: note: loop unrolled 7 times" "loop2_unroll" } } */

  #pragma GCC unroll 8
  for (unsigned long i = 1; i <= j; ++i)
    bar(i);
  /* { dg-final { scan-rtl-dump "26:.*: note: loop unrolled 7 times" "loop2_unroll" } } */

  #pragma GCC unroll 7
  for (unsigned long i = 1; i <= j; ++i)
    bar(i);
  /* { dg-final { scan-rtl-dump "31:.*: note: loop unrolled 3 times" "loop2_unroll" } } */

  unsigned long i = 0;
  #pragma GCC unroll 3
  do {
    bar(i);
  } while (++i < 9);
  /* { dg-final { scan-rtl-dump "3\[79\]:.*: note: loop unrolled 2 times" "loop2_unroll" } } */
}
