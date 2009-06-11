/* Excess precision tests.  Verify excess precision doesn't affect
   actual types.  */
/* { dg-do compile } */
/* { dg-options "-mfpmath=387 -fexcess-precision=standard" } */

float f;
double d;

void
test_types (void)
{
  float *fp;
  double *dp;
#define CHECK_FLOAT(E) fp = &(typeof(E)){0}
#define CHECK_DOUBLE(E) dp = &(typeof(E)){0}
  CHECK_FLOAT (f + f);
  CHECK_DOUBLE (d + d);
  CHECK_FLOAT (f * f / f);
  CHECK_DOUBLE (d * d / d);
  CHECK_FLOAT (f ? f - f : f);
  CHECK_DOUBLE (d ? d - d : d);
}
