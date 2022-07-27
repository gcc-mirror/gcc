/* PR target/87496 */
/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mlong-double-64 -mabi=ibmlongdouble -Wno-psabi" } */

int i;

/* { dg-error "'-mabi=ibmlongdouble' requires '-mlong-double-128'" "PR87496" { target *-*-* } 0 } */
