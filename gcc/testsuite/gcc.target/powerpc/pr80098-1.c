/* { dg-do compile { target { powerpc64*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9 -mno-power9-vector -mpower9-minmax" } */

int i;

/* { dg-error "'-mno-power9-vector' turns off '-mpower9-minmax'" "PR80098" { target *-*-* } 0 } */
