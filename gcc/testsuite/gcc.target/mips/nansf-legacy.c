/* { dg-do compile } */
/* { dg-options "-mnan=legacy -EB" } */

float fs = __builtin_nansf ("");

/* { dg-final { scan-assembler "\t\.nan\tlegacy\n" } } */
/* { dg-final { scan-assembler "\t\.word\t2147483647\n" } } */
