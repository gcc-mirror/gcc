/* { dg-do compile { target avr_tiny } } */
/* { dg-options "-mmcu=avrtiny" } */

const __memx char ascmonth[] = "Jan"; /* { dg-error "not supported" } */
