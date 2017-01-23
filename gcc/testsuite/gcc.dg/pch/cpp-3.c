/* PR preprocessor/36649 */
/* { dg-do compile } */
/* { dg-options "-H -I." } */
/* { dg-message "cpp-3.h\[^\n\]*(\n\[^\n\]*cpp-3.c)?\n\[^\n\]*cpp-3a.h\n\[^\n\]*cpp-3b.h" "" { target *-*-* } 0 } */

#include "cpp-3.h"
#include "cpp-3a.h"

int
main (void)
{
  return 0;
}
