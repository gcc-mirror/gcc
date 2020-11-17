/* Test including <float.h> then <math.h> does not result in errors
   from duplicate NAN and INFINITY macros.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

#include <float.h>
#include <math.h>
