/* { dg-do compile } */
/* { dg-options {-O1 -mno-soft-stack} } */

int
main(void)
{
  return !(__builtin_alloca(100) != __builtin_alloca(10));
  /* { dg-message {sorry, unimplemented: target cannot support alloca} {} { target *-*-* } .-1 } */
}
