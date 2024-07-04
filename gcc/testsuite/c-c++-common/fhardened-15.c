/* { dg-do compile { target *-*-linux* *-*-gnu* } } */
/* { dg-require-stack-check "specific" } */
/* { dg-options "-fhardened -O -fstack-check" } */

/* { dg-warning ".-fstack-clash-protection. is not enabled by .-fhardened. because .-fstack-check. was specified" "" { target *-*-* } 0 } */
