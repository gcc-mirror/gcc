/* { dg-do compile } */
/* { dg-options "-mnan=2008 -EB" } */

float f = __builtin_nanf ("");

/* { dg-final { scan-assembler "\t\.nan\t2008\n" } } */
/* { dg-final { scan-assembler "\t\.word\t2143289344\n" } } */
