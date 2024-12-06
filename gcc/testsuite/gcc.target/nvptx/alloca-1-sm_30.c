/* { dg-do compile } */
/* { dg-options -mno-soft-stack } */
/* { dg-additional-options -march=sm_30 } */

void sink(void *);

void f(void)
{
  sink(__builtin_alloca(123));
  /* { dg-message {sorry, unimplemented: target cannot support alloca} {} { target *-*-* } .-1 } */
}
