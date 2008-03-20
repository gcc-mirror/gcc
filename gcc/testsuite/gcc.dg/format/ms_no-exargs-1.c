/* Test for warnings for extra format arguments being disabled by
   -Wno-format-extra-args.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=gnu99 -Wformat -Wno-format-extra-args" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (int i)
{
  printf ("foo", i);
  printf ("%1$d", i, i);
}
