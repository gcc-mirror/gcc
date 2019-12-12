/* PR middle-end/40340 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall -Wno-system-headers -fno-tree-dse" } */

#include "pr40340.h"

static inline
__attribute__ ((always_inline))
void
test (char *p)
{
  memset (p, 0, 6);
}

int
main (void)
{
  char buf[4];
  test (buf);
  return 0;
}

/* { dg-warning "\\\[-Warray-bounds|-Wstringop-overflow" "" { target *-*-* } 10 } */
/* { dg-message "file included" "In file included" { target *-*-* } 0 } */
