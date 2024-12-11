/* { dg-do compile } */
/* { dg-options {-O0 -mno-soft-stack} } */

void sink(void *);

void f(void)
{
  sink(__builtin_alloca(123));
  /* { dg-message {sorry, unimplemented: target cannot support alloca} {} { target *-*-* } .-1 } */
}
