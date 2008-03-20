/* Test for scanf formats.  Formats using extensions to the standard
   should be rejected in strict pedantic mode.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:1999 -pedantic -Wformat" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (int *ip, long long int *llp, wchar_t *ls)
{
  /* The length modifiers q and L as applied to integer formats are
     extensions.
  */
  scanf ("%qd", llp); /* { dg-warning "unknown|format" "%q is unsupported" } */
  scanf ("%Ld", llp); /* { dg-warning "unknown|format" "%L is unsupported" } */
  /* The conversion specifiers C and S are X/Open extensions.  */
  scanf ("%C", ls); /* { dg-warning "C" "scanf %C" } */
  scanf ("%S", ls); /* { dg-warning "C" "scanf %S" } */
  /* The use of operand number $ formats is an X/Open extension.  */
  scanf ("%1$d", ip); /* { dg-warning "C" "scanf $ format" } */
  /* glibc also supports flags ' and I on scanf formats as an extension.  */
  scanf ("%'d", ip); /* { dg-warning "C" "scanf ' flag" } */
  scanf ("%Id", (ssize_t *)ip); /* { dg-warning "C" "scanf I flag" } */
}
