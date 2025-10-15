/* { dg-do compile } */
/* PR c/102846 */

#define f(a) ((void)0)

void g(int a)
{
  (f)(a); /* { dg-error "was not declared|undeclared" "undeclared" } */
  /* { dg-message "is a function-like macro and might be used incorrectly" "" { target *-*-* } .-1 } */
}
