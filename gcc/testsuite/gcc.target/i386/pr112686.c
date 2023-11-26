/* PR target/112686 */
/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target split_stack } */
/* { dg-options "-fsplit-stack -mcmodel=large" } */

void foo (void) {}
