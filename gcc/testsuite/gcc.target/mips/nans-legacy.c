/* { dg-do compile } */
/* { dg-options "-mnan=legacy -EB" } */

double ds = __builtin_nans ("");

/* { dg-final { scan-assembler "\t\\.nan\tlegacy\n" } } */
/* { dg-final { scan-assembler "\t\\.word\t2147483647\n\t\\.word\t-1\n" } } */
