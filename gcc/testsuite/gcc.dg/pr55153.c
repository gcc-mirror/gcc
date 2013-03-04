/* PR tree-optimization/55153 */
/* { dg-do compile } */
/* { dg-options "-O -fsched2-use-superblocks -fschedule-insns2" } */
/* { dg-require-effective-target scheduling } */

extern int a[];

void
foo (void)
{
  __builtin_prefetch (a, 0, 0);
}
