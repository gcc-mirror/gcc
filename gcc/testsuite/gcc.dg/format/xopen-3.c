/* Test for warnings for $ operand numbers after ordinary formats.
   Bug c/15444 from james-gcc-bugzilla-501qll3d at and dot org.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#include "format.h"

void
foo (int i)
{
  printf ("%d%2$d", i); /* { dg-warning "used after format" "mixing $ and non-$ formats" } */
  printf ("%d%*1$d", i, i); /* { dg-warning "used after format" "mixing $ and non-$ formats" } */
  printf ("%d%.*1$d", i, i); /* { dg-warning "used after format" "mixing $ and non-$ formats" } */
}
