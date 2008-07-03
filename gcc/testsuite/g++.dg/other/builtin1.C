/* { dg-do compile } */

void foo()
{
  __builtin_isless (foo, 0); /* { dg-error "non-floating-point arguments" } */
}
