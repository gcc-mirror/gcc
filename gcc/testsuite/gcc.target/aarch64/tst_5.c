/* { dg-do compile } */
/* { dg-options "-O2" } */

int
f255 (int x)
{
  if (x & 255)
    return 1;
  return x;
}

int
f65535 (int x)
{
  if (x & 65535)
    return 1;
  return x;
}

/* { dg-final { scan-assembler "tst\t(x|w)\[0-9\]+,\[ \t\]*255" } } */
/* { dg-final { scan-assembler "tst\t(x|w)\[0-9\]+,\[ \t\]*65535" } } */
