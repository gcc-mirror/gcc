/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-mdejagnu-cpu=power7 -O0" } */
/* { dg-require-effective-target powerpc_vsx } */

long foo (void) { return 0; }
