/* Test including <math.h> then <float.h> does not result in errors
   from duplicate NAN and INFINITY macros.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

#include <math.h>
#include <float.h>
