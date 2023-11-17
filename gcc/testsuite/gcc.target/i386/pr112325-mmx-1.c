/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-msse2 -O2 -fdump-tree-slp2" } */
/* { dg-final { scan-tree-dump-times ".REDUC_IOR" 3 "slp2" } } */

short
foo1 (short* a)
{
  short sum = 0;
  sum |= a[0];
  sum |= a[1];
  sum |= a[2];
  sum |= a[3];
  return sum;
}

char
foo2 (char* a)
{
  char sum = 0;
  sum |= a[0];
  sum |= a[1];
  sum |= a[2];
  sum |= a[3];
  sum |= a[4];
  sum |= a[5];
  sum |= a[6];
  sum |= a[7];
  return sum;
}

char
foo3 (char* a)
{
  char sum = 0;
  sum |= a[0];
  sum |= a[1];
  sum |= a[2];
  sum |= a[3];
  return sum;
}
