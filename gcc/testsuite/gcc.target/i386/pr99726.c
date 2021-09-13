/* { dg-options "-flive-patching=inline-clone -mavx512f -O2 -ftree-loop-vectorize -ftrapv" } */
/* { dg-additional-options "-floop-nest-optimize" { target fgraphite } } */

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
