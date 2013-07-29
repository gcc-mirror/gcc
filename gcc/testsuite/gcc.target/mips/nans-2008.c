/* { dg-do compile } */
/* { dg-options "-mnan=2008 -EB" } */

double ds = __builtin_nans ("");

/* { dg-final { scan-assembler "\t\.nan\t2008\n" } } */
/* { dg-final { scan-assembler "\t\.word\t2146697216\n\t.word\t0\n" } } */
