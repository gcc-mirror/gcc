/* PR driver/71063 */
/* Test we don't ICE.  */
/* { dg-do compile } */
/* { dg-options "--help=^" } */

/* { dg-error "missing argument to" "" { target *-*-* } 0 } */
