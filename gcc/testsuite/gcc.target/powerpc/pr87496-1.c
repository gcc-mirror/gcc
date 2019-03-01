/* PR target/87496 */
/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target longdouble128 } */
/* { dg-options "-O2 -mdejagnu-cpu=power7 -mabi=ieeelongdouble -mno-popcntd -Wno-psabi" } */

int i;

/* { dg-error "'-mabi=ieeelongdouble' requires full ISA 2.06 support" "PR87496" { target *-*-* } 0 } */
