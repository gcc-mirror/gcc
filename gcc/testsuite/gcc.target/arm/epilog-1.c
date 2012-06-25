/* Register liveness information from epilgoue enables peephole optimization. */
/* { dg-do compile } */
/* { dg-options "-mthumb -Os" } */
/* { dg-require-effective-target arm_thumb2_ok } */

volatile int g_k;
extern void bar(int, int, int, int);

int foo(int a, int b, int c, int d)
{
  if (g_k & 4) c++;
  bar (a, b, c, d);
  return 0;
}

/* { dg-final { scan-assembler-times "lsls.*#29" 1 } } */
/* { dg-final { scan-assembler-not "tst" } } */
