/* PR target/96166 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3 -mtune=generic -masm=att" } */
/* { dg-final { scan-assembler "rolq\\s\\\$32, \\\(%\[re]di\\\)" } } */

static inline void
swap (int *x, int *y)
{
  int tmp = *x;
  *x = *y;
  *y = tmp;
}

void
bar (int (*x)[2])
{
  int y[2];
  __builtin_memcpy (&y, x, sizeof *x);
  swap (&y[0], &y[1]);
  __builtin_memcpy (x, &y, sizeof *x);
}
