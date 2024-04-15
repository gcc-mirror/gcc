/* PR target/114606 */
/* { dg-do compile { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-fhardened -O2 -fcf-protection=none" } */

#ifdef __CET__
# error "-fcf-protection enabled when it should not be"
#endif

/* { dg-warning ".-fcf-protection=full. is not enabled by .-fhardened. because it was specified" "" { target *-*-* } 0 } */
