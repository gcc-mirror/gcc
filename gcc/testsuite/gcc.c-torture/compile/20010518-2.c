/* { dg-do compile } */

/* This test fails on HC11/HC12 when it is compiled without -mshort because 
   the array is too large (INT_MAX/2 > 64K).  Force to use 16-bit ints
   for it.  */
/* { dg-options "-w -mshort" { target m6811-*-* m6812-*-* } } */

/* Large static storage.  */

#include <limits.h>

static volatile char chars_1[INT_MAX / 2];
static volatile char chars_2[1];

int
foo (void)
{
  chars_1[10] = 'y';
  chars_2[0] = 'x';
}
