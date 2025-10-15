/* { dg-do compile } */
/* PR c/102846 */

#include <assert.h>

void g(int a)
{
  (assert)(a); /* { dg-error "was not declared|undeclared" "undeclared" } */
  /* { dg-message "is a function-like macro and might be used incorrectly" "macro" { target *-*-* } .-1 } */
  /* { dg-bogus "is defined in header" "header" { target *-*-* } .-2  }*/
}
