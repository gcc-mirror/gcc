/* Verify macros named with UTF-8 are output in -dD output with UCNs.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c99 -dD" } */
/* { dg-final { scan-file ucnid-13-utf8.i "\\\\U000000c1" } } */
#define Á 1
