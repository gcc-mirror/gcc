/* Verify that __builtin_nan("") produces a constant matches
   architecture specification. */
/* { dg-do compile } */

double d = __builtin_nan ("");

/* { dg-final { scan-assembler "\t.long\t-1\n\t.long\t2146959359\n" } } */
