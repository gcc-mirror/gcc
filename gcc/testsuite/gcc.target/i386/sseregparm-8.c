/* { dg-do compile } */
/* { dg-options "-mno-sse" } */
/* { dg-require-effective-target ia32 } */

float essef(float) __attribute__((sseregparm));
double essed(double) __attribute__((sseregparm));
float __attribute__((sseregparm)) ssef(float f);
double __attribute__((sseregparm)) ssed(double d);
extern double d;
extern float f;
void test(void)
{
  f = essef(f); /* { dg-error "SSE" } */
  d = essed(d); /* { dg-error "SSE" } */
  f = ssef(f); /* { dg-error "SSE" } */
  d = ssed(d); /* { dg-error "SSE" } */
}
