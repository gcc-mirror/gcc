/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef int __attribute__((vector_size(8))) vec;

void
store_adjusted(vec *out, vec x, vec y)
{
  out[400] = x;
  out[401] = y;
  out[402] = y;
  out[403] = x;
}

/* { dg-final { scan-assembler {add\tx[0-9]+, x[0-9]+, 3200} } } */
/* { dg-final { scan-assembler {stp\td[0-9]+, d[0-9]+, \[x[0-9]+\]} } } */
/* { dg-final { scan-assembler {stp\td[0-9]+, d[0-9]+, \[x[0-9]+, 16\]} } } */
/* { dg-final { scan-assembler-not "str\t" } } */
