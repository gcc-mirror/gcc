/* { dg-do compile { target *-*-darwin[912]* } } */
/* { dg-options "-fcommon" } */

/* In all cases, common has a max alignment of 2^15.  */
int badcommon __attribute__ ((aligned (65536))); /* { dg-error "common variables must have an alignment" } */
