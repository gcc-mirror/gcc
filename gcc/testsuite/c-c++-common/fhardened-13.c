/* { dg-do compile { target { { *-*-linux* *-*-gnu* } && pie } } } */
/* { dg-options "-fhardened -O" } */

#if __PIE__ != 2
# error "-fPIE not enabled"
#endif
