/* PR rtl-optimization/127661 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-thread-jumps -fdump-rtl-cprop1" } */

void f (int);

#define T(n) if (v == n) f (n);

void
foo (int v)
{
  T (0) T (1) T (2) T (3) T (4) T (5) T (6) T (7)
}

/* Jump bypassing must not add copies of the compares that it skips.  */
/* { dg-final { scan-rtl-dump "JUMP-BYPASS" "cprop1" } } */
/* { dg-final { scan-rtl-dump-times {\(compare:} 8 "cprop1" } } */
