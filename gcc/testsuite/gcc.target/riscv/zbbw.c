/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64 -O2" } */

int
clz (int i)
{
  return __builtin_clz (i);
}

int
ctz (int i)
{
  return __builtin_ctz (i);
}

int
popcount (int i)
{
  return __builtin_popcount (i);
}


/* { dg-final { scan-assembler-times "clzw" 1 } } */
/* { dg-final { scan-assembler-times "ctzw" 1 } } */
/* { dg-final { scan-assembler-times "cpopw" 1 } } */
