/* PR target/114606 */
/* { dg-do compile { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-fhardened -O2" } */

#if __CET__ != 3
# error "-fcf-protection not enabled"
#endif

/* { dg-bogus ".-fcf-protection=full. is not enabled by .-fhardened. because it was specified" "" { target *-*-* } 0 } */
