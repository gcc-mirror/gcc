/* PR driver/117739 */
/* { dg-do compile { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-fhardened -O -Wl,-z,lazy -Whardened" } */

/* { dg-warning "linker hardening options not enabled" "" { target *-*-* } 0 } */
