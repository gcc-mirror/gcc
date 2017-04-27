/* PR middle-end/40340 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall -Wno-system-headers" } */

#define TEST2
#include "pr40340.h"

int
main (void)
{
  test2 ();
  return 0;
}

/* { dg-bogus "overflow" "" { target *-*-* } 10 } */
