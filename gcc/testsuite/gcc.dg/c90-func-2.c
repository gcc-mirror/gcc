/* Test that we don't pedwarn about __func__ predefined identifier in
   a system header in C90 pedantic mode.  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

#include "c90-func-2.h"

void
foo (void)
{
  const char *s = FN;
}
