/* { dg-do compile } */
/* { dg-options {-O0 -mno-soft-stack} } */

void sink(void *);

void f(int s)
{
  char a[s];
  /* { dg-message {sorry, unimplemented: target cannot support alloca} {} { target *-*-* } .-1 } */
  sink(a);
}
