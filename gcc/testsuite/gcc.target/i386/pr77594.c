/* PR middle-end/77594 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

int
foo (int a, int *b)
{
  return __builtin_sub_overflow (0, a, b);
}

/* { dg-final { scan-assembler-times "\tjn?o\t" 1 } } */
