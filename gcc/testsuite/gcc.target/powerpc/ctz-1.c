/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9modulo_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-mcpu=power9 -O2" } */

int i_trailing_zero (int a) { return __builtin_ctz (a); }
int l_trailing_zero (long a) { return __builtin_ctzl (a); }
int ll_trailing_zero (long long a) { return __builtin_ctzll (a); }

/* { dg-final { scan-assembler     "cnttzw " } } */
/* { dg-final { scan-assembler     "cnttzd " } } */
/* { dg-final { scan-assembler-not "cntlzw " } } */
/* { dg-final { scan-assembler-not "cntlzd " } } */
