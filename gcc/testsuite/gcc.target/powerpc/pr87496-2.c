/* PR target/87496 */
/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power7 -mlong-double-64 -mabi=ieeelongdouble -Wno-psabi" } */

int i;

/* { dg-error "'-mabi=ieeelongdouble' requires '-mlong-double-128'" "PR87496" { target *-*-* } 0 } */
