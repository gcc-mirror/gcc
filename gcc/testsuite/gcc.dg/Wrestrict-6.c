/* PR tree-optimization/83640 - ice in generic_overlap, at
   gimple-ssa-warn-restrict.c:814
   Test to verify that a pointer offset range whose upper bound is less
   than its lower bound (when interpreted as a signed integer) is handled
   correctly.
   { dg-do compile }
   { dg-options "-O2 -Wrestrict" }  */

#include "range.h"

extern char* strcpy (char*, const char*);
extern char* stpcpy (char*, const char*);

void sink (void*);

void warn_2_smax_p2 (void)
{
  char a[7] = "01234";

  char *d = a;

  ptrdiff_t i = UR (2, DIFF_MAX + (size_t)2);

  strcpy (d, d + i);          /* { dg-warning "accessing between 0 and 4 bytes at offsets 0 and \\\[2, 7] may overlap up to 2 bytes at offset 2" } */

  sink (d);
}

void nowarn_3_smax_p2 (void)
{
  char a[7] = "12345";

  char *d = a;

  ptrdiff_t i = UR (3, DIFF_MAX + (size_t)2);

  strcpy (d, d + i);

  sink (d);
}

void warn_2u_smax_p2 (void)
{
  char a[7] = "23456";

  char *d = a;

  size_t i = UR (2, DIFF_MAX + (size_t)2);

  strcpy (d, d + i);          /* { dg-warning "accessing between 0 and 4 bytes at offsets 0 and \\\[2, 7] may overlap up to 2 bytes at offset 2" } */

  sink (d);
}

void nowarn_3u_smax_p2 (void)
{
  char a[7] = "34567";

  char *d = a;

  size_t i = UR (3, DIFF_MAX + (size_t)2);

  strcpy (d, d + i);

  sink (d);
}
