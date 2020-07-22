/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef double __attribute__((vector_size(16))) vec;

void
store_adjusted(vec *out, vec x, vec y)
{
  out[100] = x;
  out[101] = y;
  out[102] = y;
  out[103] = x;
}

/* { dg-final { scan-assembler {add\tx[0-9]+, x[0-9]+, 1600} } } */
/* { dg-final { scan-assembler {stp\tq[0-9]+, q[0-9]+, \[x[0-9]+\]} } } */
/* { dg-final { scan-assembler {stp\tq[0-9]+, q[0-9]+, \[x[0-9]+, 32\]} } } */
/* { dg-final { scan-assembler-not "str\t" } } */
