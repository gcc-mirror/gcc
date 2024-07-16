/* { dg-do compile } */
/* { dg-additional-options "-Ofast -fcommon -fvect-cost-model=dynamic --param vect-partial-vector-usage=1" } */
/* { dg-additional-options "-mavx512vl" { target avx512vl } } */

/* To trigger the bug costing needs to determine that aligning the A170
   accesses with a prologue is good and there should be a vectorized
   epilogue with a smaller vector size, re-using the vector accumulator
   from the vectorized main loop that's statically known to execute
   but the epilogue loop is not.  */

static unsigned char xl[192];
unsigned char A170[192*3];

void jerate (unsigned char *, unsigned char *);
float foo (unsigned n)
{
  jerate (xl, A170);

  unsigned i = 32;
  int kr = 1;
  float sfn11s = 0.f;
  float sfn12s = 0.f;
  do
    {
      int krm1 = kr - 1;
      long j = krm1;
      float a = (*(float(*)[n])A170)[j];
      float b = (*(float(*)[n])xl)[j];
      float c = a * b;
      float d = c * 6.93149983882904052734375e-1f;
      float e = (*(float(*)[n])A170)[j+48];
      float f = (*(float(*)[n])A170)[j+96];
      float g = d * e;
      sfn11s = sfn11s + g;
      float h = f * d;
      sfn12s = sfn12s + h;
      kr++;
    }
  while (--i != 0);
  float tem = sfn11s + sfn12s;
  return tem;
}
