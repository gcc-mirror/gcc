/* { dg-do compile } */
/* { dg-options "-mnan=legacy -EB" } */

double d = __builtin_nan ("");

/* { dg-final { scan-assembler "\t\\.nan\tlegacy\n" } } */
/* { dg-final { scan-assembler "\t\\.word\t2146959359\n\t\\.word\t(?:-1|4294967295)\n" } } */
