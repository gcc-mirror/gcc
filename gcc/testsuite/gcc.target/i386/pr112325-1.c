/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512bw -O2 -mtune=generic -mprefer-vector-width=512 -fdump-tree-slp2" } */
/* { dg-final { scan-tree-dump-times ".REDUC_PLUS" 3 "slp2" } } */
/* { dg-final { scan-tree-dump-times ".REDUC_IOR" 4 "slp2" } } */

int
__attribute__((noipa))
plus_v4si (int* a)
{
  int sum = 0;
  sum += a[0];
  sum += a[1];
  sum += a[2];
  sum += a[3];
  return sum;
}

short
__attribute__((noipa))
plus_v8hi (short* a)
{
  short sum = 0;
  sum += a[0];
  sum += a[1];
  sum += a[2];
  sum += a[3];
  sum += a[4];
  sum += a[5];
  sum += a[6];
  sum += a[7];
  return sum;
}

long long
__attribute__((noipa))
plus_v8di (long long* a)
{
  long long sum = 0;
  sum += a[0];
  sum += a[1];
  sum += a[2];
  sum += a[3];
  sum += a[4];
  sum += a[5];
  sum += a[6];
  sum += a[7];
  return sum;
}

int
__attribute__((noipa))
ior_v4si (int* a)
{
  int sum = 0;
  sum |= a[0];
  sum |= a[1];
  sum |= a[2];
  sum |= a[3];
  return sum;
}

short
__attribute__((noipa))
ior_v8hi (short* a)
{
  short sum = 0;
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

long long
__attribute__((noipa))
ior_v8di (long long* a)
{
  long long sum = 0;
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
__attribute__((noipa))
ior_v16qi (char* a)
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
  sum |= a[8];
  sum |= a[9];
  sum |= a[10];
  sum |= a[11];
  sum |= a[12];
  sum |= a[13];
  sum |= a[14];
  sum |= a[15];
  return sum;
}
