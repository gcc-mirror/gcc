/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */

typedef double __m512d __attribute__ ((__vector_size__ (64)));

extern __m512d _ZGVeN8v_func (__m512d);

double
func_vlen8 (double x)
{
  __m512d mx, mr;

  mx[0] = mx[1] = mx[2] = mx[3] = mx[4] = mx[5] = mx[6] = mx[7] = x;
  mr = _ZGVeN8v_func (mx);

  return (double) mr[0];
}
