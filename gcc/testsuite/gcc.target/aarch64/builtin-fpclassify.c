/* This file checks the code generation for the new __builtin_fpclassify.
   because checking the exact assembly isn't very useful, we'll just be checking
   for the presence of certain instructions and the omition of others. */
/* { dg-options "-O2" } */
/* { dg-do compile } */
/* { dg-final { scan-assembler-not "\[ \t\]?fabs\[ \t\]?" } } */
/* { dg-final { scan-assembler-not "\[ \t\]?fcmp\[ \t\]?" } } */
/* { dg-final { scan-assembler-not "\[ \t\]?fcmpe\[ \t\]?" } } */
/* { dg-final { scan-assembler "\[ \t\]?ubfx\[ \t\]?" } } */

#include <stdio.h>
#include <math.h>

/*
 fp_nan = args[0];
 fp_infinite = args[1];
 fp_normal = args[2];
 fp_subnormal = args[3];
 fp_zero = args[4];
*/

int f(double x) { return __builtin_fpclassify(0, 1, 4, 3, 2, x); }
