/* { dg-do compile } */
/* { dg-options {-O1 -mno-soft-stack} } */
/* { dg-additional-options -march=sm_30 } */

int
main(void)
{
  return !(__builtin_alloca(100) != __builtin_alloca(10));
  /* { dg-message {sorry, unimplemented: dynamic stack allocation not supported} {} { target *-*-* } .-1 } */
}
