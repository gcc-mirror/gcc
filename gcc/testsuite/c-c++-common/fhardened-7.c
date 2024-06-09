/* { dg-do compile { target { { *-*-linux* *-*-gnu* } && pie } } } */
/* { dg-options "-fhardened -O -fpie" } */

/* -fpie takes precedence over -fhardened */
#if __PIE__ != 1
# error "__PIE__ != 1"
#endif
