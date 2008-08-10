/* { dg-do compile } */
/* { dg-options "-pedantic-errors -Wsystem-headers" } */
#include "pr36901-system.h"
void foo(void)
{
  int s = sc;
}
/* { dg-message "from " "In file included" { target *-*-* } 0 } */
/* { dg-error "ordered comparison of pointer with integer zero" "pedantic error" { target *-*-* } 0 } */
