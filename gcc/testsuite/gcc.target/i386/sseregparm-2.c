/* { dg-do compile } */
/* { dg-options "-mno-sse" } */
/* { dg-require-effective-target ilp32 } */

float essef(float) __attribute__((sseregparm));
double essed(double) __attribute__((sseregparm));
float __attribute__((sseregparm, noinline)) ssef(float f) { return f; } /* { dg-warning "SSE" } */
double __attribute__((sseregparm, noinline)) ssed(double d) { return d; } /* { dg-warning "SSE" } */
extern double d;
extern float f;
void test(void)
{
  f = essef(f); /* { dg-warning "SSE" } */
  d = essed(d); /* { dg-warning "SSE" } */
  f = ssef(f); /* { dg-warning "SSE" } */
  d = ssed(d); /* { dg-warning "SSE" } */
}
