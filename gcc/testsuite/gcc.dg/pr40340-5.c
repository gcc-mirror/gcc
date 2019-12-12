/* PR middle-end/40340 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall -Wsystem-headers -g -fno-tree-dse" } */

#define TEST3
#include "pr40340.h"

int
main (void)
{
  char buf[4];
  test3 (buf);
  return 0;
}

/* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" "" { target *-*-* } 10 } */
/* { dg-message "file included" "In file included" { target *-*-* } 0 } */
