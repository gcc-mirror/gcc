/* { dg-do compile } */
/* { dg-options "-march=avrtiny" } */

const __memx char ascmonth[] = "Jan"; /* { dg-error "not supported" } */
