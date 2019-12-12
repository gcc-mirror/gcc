/* { dg-do compile { target { powerpc64*-*-* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mno-power8-vector -mcrypto" } */

int i;

/* { dg-error "'-mno-power8-vector' turns off '-mcrypto'"      "PR80098" { target *-*-* } 0 } */
