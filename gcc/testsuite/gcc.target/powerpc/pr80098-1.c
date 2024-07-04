/* { dg-do compile { target { powerpc64*-*-* } } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mno-vsx -mpower9-minmax" } */

int i;

/* { dg-error "'-mno-vsx' turns off '-mpower9-minmax'" "PR80098" { target *-*-* } 0 } */
