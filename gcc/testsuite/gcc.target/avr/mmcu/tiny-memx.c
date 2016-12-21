/* { dg-do compile } */
/* { dg-options "-mmcu=avrtiny" } */

const __memx char ascmonth[] = "Jan"; /* { dg-error "not supported" } */
