/* { dg-do compile { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-fhardened -fstack-protector" } */

#ifdef __SSP_STRONG__
# error "-fstack-protector-strong enabled when it should not be"
#endif
#if !defined(__SSP__) && !defined(__hppa__)
# error "-fstack-protector not enabled"
#endif

/* { dg-warning ".-fstack-protector-strong. is not enabled" "" { target *-*-* } 0 } */
/* { dg-warning "._FORTIFY_SOURCE. is not enabled" "" { target *-*-* } 0 } */
