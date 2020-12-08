/* { dg-do compile } */

typedef double v2df __attribute__((vector_size(2*sizeof(double))));

v2df foo (double *y)
{
  v2df x = (v2df){ 1.0, 2.0 };
  x[0] = *y;
  return x;
}
