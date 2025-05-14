/* { dg-do compile } */
/* { dg-options {-O0 -mno-soft-stack} } */
/* { dg-additional-options -march=sm_30 } */

void f(void)
{
  char *a = __builtin_alloca(123);
  /* { dg-message {sorry, unimplemented: dynamic stack allocation not supported} {} { target *-*-* } .-1 } */
  a[0] = 0;
}
