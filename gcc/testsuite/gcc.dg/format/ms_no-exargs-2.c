/* Test for warnings for extra format arguments being disabled by
   -Wno-format-extra-args.  Test which warnings still apply with $
   operand numbers.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=gnu99 -Wformat -Wno-format-extra-args" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (int i, int *ip, va_list va)
{
  printf ("%3$d%1$d", i, i, i); /* { dg-warning "before used" "unused $ operand" } */
  printf ("%2$d%1$d", i, i, i);
  vprintf ("%3$d%1$d", va); /* { dg-warning "before used" "unused $ operand" } */
  /* With scanf formats, gaps in the used arguments are allowed only if the
     arguments are all pointers.  In such a case, should only give the lesser
     warning about unused arguments rather than the more serious one about
     argument gaps.  */
  scanf ("%3$d%1$d", ip, ip, ip);
  /* If there are non-pointer arguments unused at the end, this is also OK.  */
  scanf ("%3$d%1$d", ip, ip, ip, i);
  scanf ("%3$d%1$d", ip, i, ip); /* { dg-warning "before used" "unused $ scanf non-pointer operand" } */
  /* Can't check the arguments in the vscanf case, so should suppose the
     lesser problem.  */
  vscanf ("%3$d%1$d", va);
}
