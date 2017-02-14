/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef __Float64x1_t float64x1_t;

typedef long long int64_t;

extern int64_t bar (float64x1_t f);

int
foo (void)
{
  float64x1_t f = { 3.14159265358979311599796346854 };
  int64_t c = 0x400921FB54442D18;
  int64_t r;
  r = bar (f);
  return r == c;
}
