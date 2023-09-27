/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64" } */

int
ctz (int i)
{
  int res = __builtin_ctz (i);
  return res&0xffff;
}

/* { dg-final { scan-assembler-times {\mctzw} 1 } } */
/* { dg-final { scan-assembler-not {\mandi} } } */
