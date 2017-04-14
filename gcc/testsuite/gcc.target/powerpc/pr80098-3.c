/* { dg-do compile { target { powerpc64*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mcpu=power7 -mno-vsx -mdirect-move -mcrypto" } */

int i;

/* { dg-error "-mno-vsx turns off -mdirect-move" "PR80098" { target *-*-* } 0 } */
/* { dg-error "-mno-vsx turns off -mcrypto"      "PR80098" { target *-*-* } 0 } */
