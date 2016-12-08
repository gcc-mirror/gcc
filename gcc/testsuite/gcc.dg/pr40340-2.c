/* PR middle-end/40340 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall -Wno-system-headers" } */

#include "pr40340.h"

int
main (void)
{
  char buf[4];
  memset (buf, 0, 6);
  return 0;
}

/* { dg-warning "writing" "" { target *-*-* } 10 } */
/* { dg-message "file included" "In file included" { target *-*-* } 0 } */
