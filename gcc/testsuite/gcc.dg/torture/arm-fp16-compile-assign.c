/* { dg-do compile { target arm*-*-* } } */
/* { dg-options "-mfp16-format=ieee" } */

/* Test basic assignments and conversions for __fp16.  */

__fp16 h0 = -1.0;
__fp16 h1 = 0.0;
__fp16 h2 = 1234.0;
__fp16 h3 = 42.0;
float f1 = 2.0;
float f2 = -999.9;

void f (__fp16 *p)
{
  __fp16 t;

  h0 = 1.0;
  h1 = h2;
  h2 = f1;
  f2 = h2;

  t = *p;
  *p = h3;
  h3 = t;
}

/* Make sure we are not falling through to undefined libcalls.  */
/* { dg-final { scan-assembler-not "__truncsfhf" } } */
/* { dg-final { scan-assembler-not "__extendhfsf" } } */
