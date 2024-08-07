/* { dg-do compile { target { powerpc64*-*-* } } } */
/* { dg-options "-mdejagnu-cpu=power8 -mno-vsx -mcrypto" } */

int i;

/* { dg-error "'-mno-vsx' turns off '-mcrypto'"      "PR80098" { target *-*-* } 0 } */
