/* Test diagnostics for options used on their own without
   -Wformat.  -Wformat-.  */
/* Origin: Bruce Korb <bkorb@gnu.org> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-Wformat" } */

/* { dg-warning "embedded .* in format" "ignored" { target *-*-* } 0 } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
fumble (void)
{
  static char const fmt[] = "x%s\0%s\n\0abc";
  printf (fmt+4, fmt+8);
}
