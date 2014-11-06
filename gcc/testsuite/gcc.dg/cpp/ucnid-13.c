/* Verify macros named with UCNs are output in -dD output with UCNs,
   not UTF-8.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c99 -dD" } */
/* { dg-final { scan-file ucnid-13.i "\\\\U000000c1" } } */
#define \u00c1 1
