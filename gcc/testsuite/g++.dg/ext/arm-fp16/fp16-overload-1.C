/* { dg-do compile { target arm*-*-* } } */
/* { dg-options "-mfp16-format=ieee" } */

/* __fp16 values are autoconverted to float and should therefore be treated
 * just like float for overloading purposes.  */

extern int frobnify (float x);
extern int frobnify (double x);

int g (void)
{
  return frobnify ((__fp16)1.0);
}

/* { dg-final { scan-assembler "_Z8frobnifyf" } } */
/* { dg-final { scan-assembler-not " _Z8frobnifyd" } } */
