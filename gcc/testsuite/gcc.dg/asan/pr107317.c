/* PR middle-end/107317 */
/* { dg-do compile { target ilp32 } } */
/* { dg-options "-fsanitize=address -ffat-lto-objects" } */

void bar (float *, float *);

void
foo (void)		/* { dg-error "exceeds maximum" } */
{
  float a[400000000];
  float b[200000000];
  bar (a, b);
}
