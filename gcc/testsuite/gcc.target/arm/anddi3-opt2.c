/* { dg-do compile } */
/* { dg-options "-O1" } */

long long muld(long long X, long long Y)
{
  return X & ~1;
}

/* { dg-final { scan-assembler-not "and\[\\t \]+.+,\[\\t \]*.+,\[\\t \]*.+" } } */
