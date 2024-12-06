/* { dg-do compile } */
/* { dg-options -mno-soft-stack } */
/* { dg-additional-options -march=sm_30 } */

void sink(void *);

void f(int s)
{
  char a[s];
  /* { dg-message {sorry, unimplemented: target cannot support alloca} {} { target *-*-* } .-1 } */
  sink(a);
}
