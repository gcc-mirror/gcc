/* Verify macro definitions with UCNs are output in -dD output with
   the original spelling.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c99 -dD" } */
/* { dg-final { scan-file ucnid-14.i "\\\\u00c1" } } */
#define a \u00c1
