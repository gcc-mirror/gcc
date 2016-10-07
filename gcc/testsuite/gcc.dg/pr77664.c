/* PR tree-optimization/77664 */
/* { dg-do run } */
/* { dg-options "-O2" } */

#include "tree-ssa/pr77664.c"

int cnt;

__attribute__((noinline, noclone)) void
foo (void)
{
  cnt++;
}

int
main ()
{
  fn1 (65534U, 65535U, 7); if (cnt != 1) __builtin_abort ();
  fn1 (65534U, 65535U, 0); if (cnt != 1) __builtin_abort ();
  fn1 (65535U, 65535U, 1); if (cnt != 1) __builtin_abort ();
  fn1 (0, 65535U, 1); if (cnt != 2) __builtin_abort ();
  fn1 (-1, 65535U, 1); if (cnt != 2) __builtin_abort ();
  fn1 (0, 0, 1); if (cnt != 2) __builtin_abort ();
  fn2 (65534U, 65535U, 7); if (cnt != 3) __builtin_abort ();
  fn2 (65534U, 65535U, 0); if (cnt != 3) __builtin_abort ();
  fn2 (65535U, 65535U, 0); if (cnt != 4) __builtin_abort ();
  fn2 (0, 65535U, 0); if (cnt != 4) __builtin_abort ();
  fn2 (-1, 65535U, 0); if (cnt != 5) __builtin_abort ();
  fn2 (0, 0, 0); if (cnt != 6) __builtin_abort ();
  fn3 (-1, 65534U); if (cnt != 7) __builtin_abort ();
  fn3 (0, 65534U); if (cnt != 7) __builtin_abort ();
  fn3 (65534U, 65534U); if (cnt != 7) __builtin_abort ();
  fn3 (65535U, 65534U); if (cnt != 8) __builtin_abort ();
  fn3 (0, 0); if (cnt != 8) __builtin_abort ();
  fn3 (1, 0); if (cnt != 9) __builtin_abort ();
  fn4 (-1, 65534U); if (cnt != 9) __builtin_abort ();
  fn4 (0, 65534U); if (cnt != 10) __builtin_abort ();
  fn4 (65534U, 65534U); if (cnt != 11) __builtin_abort ();
  fn4 (65535U, 65534U); if (cnt != 11) __builtin_abort ();
  fn4 (0, 0); if (cnt != 12) __builtin_abort ();
  fn4 (1, 0); if (cnt != 12) __builtin_abort ();
  fn5 (-1, 65534U); if (cnt != 13) __builtin_abort ();
  fn5 (0, 65534U); if (cnt != 13) __builtin_abort ();
  fn5 (65534U, 65534U); if (cnt != 13) __builtin_abort ();
  fn5 (65535U, 65534U); if (cnt != 14) __builtin_abort ();
  fn5 (0, 0); if (cnt != 14) __builtin_abort ();
  fn5 (1, 0); if (cnt != 15) __builtin_abort ();
  fn6 (-1, 65534U); if (cnt != 15) __builtin_abort ();
  fn6 (0, 65534U); if (cnt != 16) __builtin_abort ();
  fn6 (65534U, 65534U); if (cnt != 17) __builtin_abort ();
  fn6 (65535U, 65534U); if (cnt != 17) __builtin_abort ();
  fn6 (0, 0); if (cnt != 18) __builtin_abort ();
  fn6 (1, 0); if (cnt != 18) __builtin_abort ();
  return 0;
}
