/* { dg-do compile } */
/* { dg-options "-mno-sse" } */
/* { dg-require-effective-target ilp32 } */

float essef(float) __attribute__((sseregparm));
double essed(double) __attribute__((sseregparm));
float __attribute__((sseregparm, noinline)) ssef(float f) { return f; } /* { dg-error "SSE" } */
double __attribute__((sseregparm, noinline)) ssed(double d) { return d; } /* { dg-error "SSE" } */
extern double d;
extern float f;
void test(void)
{
  f = essef(f); /* { dg-error "SSE" } */
  d = essed(d); /* { dg-error "SSE" } */
  f = ssef(f); /* { dg-error "SSE" } */
  d = ssed(d); /* { dg-error "SSE" } */
}
