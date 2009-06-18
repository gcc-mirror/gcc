/* { dg-do compile { target arm*-*-* } } */
/* { dg-options "-mfp16-format=ieee" } */

/* Test basic assignments and conversions for __fp16.  */

__fp16 h1 = 0.0;
__fp16 h2 = 1234.0;
char c1 = 1;
char c2 = 2;
short s1 = 10;
short s2 = 20;
int i1 = -100;
int i2 = -200;
long long l1 = 1000.0;
long long l2 = 2000.0;
double d1 = -10000.0;
double d2 = -20000.0;

void f (void)
{
  c1 = h1;
  h2 = c2;

  h1 = s1;
  s2 = h2;

  i1 = h1;
  h2 = i2;

  h1 = l1;
  l2 = h2;
  
  d1 = h1;
  h2 = d2;
}

/* Make sure we are not falling through to undefined libcalls.  */
/* { dg-final { scan-assembler-not "__float.ihf" } } */
/* { dg-final { scan-assembler-not "__fixhf.i" } } */
/* { dg-final { scan-assembler-not "__trunc.fhf" } } */
/* { dg-final { scan-assembler-not "__extendhf.f" } } */
