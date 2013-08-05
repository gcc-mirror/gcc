/* { dg-do compile } */
/* { dg-options "-mnan=legacy -EB" } */

float f = __builtin_nanf ("");

/* { dg-final { scan-assembler "\t\.nan\tlegacy\n" } } */
/* { dg-final { scan-assembler "\t\.word\t2143289343\n" } } */
