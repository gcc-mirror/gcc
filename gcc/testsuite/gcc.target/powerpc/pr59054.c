/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mcpu=power7 -O0 -m64" } */

long foo (void) { return 0; }

/* { dg-final { scan-assembler-not "xxlor" } } */
/* { dg-final { scan-assembler-not "stfd" } } */
/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mcpu=power7 -O0 -m64" } */

long foo (void) { return 0; }

/* { dg-final { scan-assembler-not "xxlor" } } */
/* { dg-final { scan-assembler-not "stfd" } } */
