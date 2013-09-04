/* { dg-do compile } */
/* { dg-options "-mnan=2008 -EB" } */

float fs = __builtin_nansf ("");

/* { dg-final { scan-assembler "\t\\.nan\t2008\n" } } */
/* { dg-final { scan-assembler "\t\\.word\t2141192192\n" } } */
