/* { dg-do compile } */
/* { dg-options "-pedantic-errors -Wsystem-headers" } */
#include "pr36901-system.h"
void foo(void)
{
  int s = sc;
}
/* { dg-message "from " "In file included" { target *-*-* } 0 } */
/* { dg-warning "overflow" "overflow" { target *-*-* } 0 } */
/* { dg-error "overflow" "overflow" { target *-*-* } 0 } */
/* { dg-error "#include_next is a GCC extension" "#include_next" { target *-*-* } 0 } */
