/* PR middle-end/40340 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall -Wno-system-headers -g" } */

#define TEST3
#include "pr40340.h"

int
main (void)
{
  char buf[4];
  test3 (buf);
  return 0;
}

/* { dg-bogus "will always overflow destination buffer" "" { target *-*-* } 10 } */
