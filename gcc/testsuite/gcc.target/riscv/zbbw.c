/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64" } */

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


/* { dg-final { scan-assembler-times {\mclzw} 1 } } */
/* { dg-final { scan-assembler-times {\mctzw} 1 } } */
/* { dg-final { scan-assembler-times {\mcpopw} 1 } } */
/* { dg-final { scan-assembler-not "andi\t" } } */
