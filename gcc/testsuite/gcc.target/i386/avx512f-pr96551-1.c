/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mprefer-vector-width=512" } */

unsigned int a[256];
double b[256];

void
__attribute__ ((noipa, optimize ("tree-vectorize")))
foo(void)
{
  int i;

  for (i=0; i<256; ++i)
    b[i] = a[i];
}

/* { dg-final { scan-assembler "vcvtdq2pd\[^\n\]*zmm" } } */

