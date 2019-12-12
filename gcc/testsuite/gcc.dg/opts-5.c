/* -fstack-limit should be rejected without an ICE.  */
/* { dg-do compile } */
/* { dg-options "-fstack-limit" } */

/* { dg-error "unrecognized command-line option" "" { target *-*-* } 0 } */
