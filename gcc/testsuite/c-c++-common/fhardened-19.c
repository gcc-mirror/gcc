/* PR driver/117739 */
/* { dg-do compile { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-z lazy -fhardened -O -Whardened" } */

/* { dg-warning "linker hardening options not enabled" "" { target *-*-* } 0 } */
