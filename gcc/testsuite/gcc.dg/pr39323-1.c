/* PR c/39323 */
/* { dg-do compile { target *-*-linux* } } */

int foo __attribute__ ((aligned(1 << 29))) =  20; /* { dg-error "requested alignment is too large" } */
typedef int __attribute__ ((aligned(1 << 29))) int29; /* { dg-error "requested alignment is too large" } */
