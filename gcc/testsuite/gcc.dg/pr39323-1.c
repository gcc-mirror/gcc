/* PR c/39323 - MAX_OFILE_ALIGNMENT in elfos.h is too big */
/* { dg-do compile { target *-*-linux* *-*-gnu* } } */

int foo __attribute__ ((aligned(1 << 29))) =  20; /* { dg-error "requested alignment" } */
typedef int __attribute__ ((aligned(1 << 29))) int29; /* { dg-error "requested alignment" } */
