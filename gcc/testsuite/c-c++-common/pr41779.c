/* PR41779: Wconversion cannot see throught real*integer promotions. */
/* { dg-do compile } */
/* { dg-skip-if "doubles are floats" { "avr-*-*" } { "*" } { "" } } */
/* { dg-options "-std=c99 -Wconversion" { target c } } */
/* { dg-options "-Wconversion" { target c++ } } */
/* { dg-require-effective-target large_double } */

float f(float x, unsigned short y)
{
  return x * y;
}

float f(float x, short y)
{
  return x * y;
}

float f(float x, char y)
{
  return x * y;
}

float f(float x, unsigned char y)
{
  return x * y;
}

float f(float x, int y)
{
  return x * y; /* { dg-warning "conversion" } */
}
