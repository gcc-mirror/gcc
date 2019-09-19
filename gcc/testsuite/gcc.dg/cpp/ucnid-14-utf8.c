/* Verify macro definitions with UTF-8 are output in -dD output with
   the original spelling.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c99 -dD" } */
/* { dg-final { scan-file ucnid-14-utf8.i "Á" } } */
#define a Á
