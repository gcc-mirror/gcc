/* { dg-do compile } */
/* { dg-options "-mnan=2008 -EB" } */

double d = __builtin_nan ("");

/* { dg-final { scan-assembler "\t\\.nan\t2008\n" } } */
/* { dg-final { scan-assembler "\t\\.word\t2146959360\n\t\\.word\t0\n" } } */
