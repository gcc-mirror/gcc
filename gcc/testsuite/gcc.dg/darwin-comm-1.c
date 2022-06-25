/* { dg-do compile { target *-*-darwin* } } */
/* { dg-options "-fcommon" } */

/* In all cases, common has a max alignment of 2^15.  */
int badcommon __attribute__ ((aligned (65536))); /* { dg-error "common variables must have an alignment" "" { target { *-*-darwin1[1-9]* *-*-darwin2* } } } */
/* { dg-error "requested alignment .65536. exceeds object file maximum 32768" "" { target { *-*-darwin[4-9]* *-*-darwin10* } } .-1 } */