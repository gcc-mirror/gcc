/* { dg-do compile { target { powerpc64*-*-* && lp64 } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mcpu=power7 -mno-vsx -mvsx-timode" } */

int i;

/* { dg-error "-mno-vsx turns off -mvsx-timode" "PR80098" { target *-*-* } 0 } */
