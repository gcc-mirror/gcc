/* { dg-do compile { target { powerpc64*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mcpu=power8 -mno-power8-vector -mdirect-move -mcrypto" } */

int i;

/* { dg-error "-mno-power8-vector turns off -mdirect-move" "PR80098" { target *-*-* } 0 } */
/* { dg-error "-mno-power8-vector turns off -mcrypto"      "PR80098" { target *-*-* } 0 } */
