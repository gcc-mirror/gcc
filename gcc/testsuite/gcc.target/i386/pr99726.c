/* { dg-options "-flive-patching=inline-clone -mavx512f -O2 -floop-nest-optimize -ftree-loop-vectorize -ftrapv -m32" } */

extern int a[256][1024];
int b;
long c, d;
unsigned int e;

int
main ()
{
  for (; e < d; e++)
    for (unsigned j = 1; j < c; j++)
      a[e][j] = b * a[e - 1][j + 1];
  return 0;
}
