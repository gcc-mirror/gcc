/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -msse4.1 -mstv -mno-stackrealign -fno-tree-vectorize" } */
long long a[1024];
long long b[1024];

int foo()
{
  for (int i=0; i<1024; i++)
  {
    long long t = (a[i]<<8) | (b[i]<<24);
    if (t == 0)
      return 1;
  }
  return 0;
}

/* { dg-final { scan-assembler "ptest" } } */
/* { dg-final { scan-assembler-not "pxor" } } */
