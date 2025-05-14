/* { dg-do compile } */

int
foo (int x)
{
  int *p = __builtin_alloca (x);
  /* { dg-message {sorry, unimplemented: dynamic stack allocation not supported} {} { target *-*-* } .-1 } */

  return p[2];
}
