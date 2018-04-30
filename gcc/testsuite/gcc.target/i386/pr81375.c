/* PR target/81375 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-mno-80387 -mno-sse -mfpmath=sse" } */

float foo (float a, float b)
{
  return a / b;
}
