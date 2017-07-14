/* PR target/81375 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-mno-80387 -mno-sse -mfpmath=sse" } */

/* { dg-warning "SSE instruction set disabled, using 387 arithmetics" "" { target *-*-* } 0 } */

float foo (float a, float b)
{
  return a / b;
}
