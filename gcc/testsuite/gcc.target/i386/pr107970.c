/* { dg-do compile { target ia32 } } */
/* { dg-options "-Ofast -m3dnow -msse4.1" } */

float *foo_p;

void
foo(float *__restrict q) {
  foo_p[0] = __builtin_truncf(q[0]);
  foo_p[1] = __builtin_truncf(q[1]);
}
