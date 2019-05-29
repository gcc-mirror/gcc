/* { dg-do compile }
/* { dg-options "-msse2 -O2 -fno-tree-loop-distribute-patterns -ftree-vectorize -mtune=generic" } */

#define N 256
int b[N];

void test()
{  
  int i;

  for (i = 0; i < N; i++)
    b[i] = 0;
}

/* { dg-final { scan-assembler-times "pxor" 1 } } */
