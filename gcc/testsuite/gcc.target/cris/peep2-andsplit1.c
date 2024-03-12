/* Check that "opsplit1" with AND does its job.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int al0 (int x)
{
  return x & 0x7fffffff;
}

int alN (int x)
{
  return x & 63;
}

int ar0 (int x)
{
  return x & (-32*2);
}

int arN (int x)
{
  return x & 0x80000000;
}

/* { dg-final { scan-assembler-not "\[ \t\]and" } } */
