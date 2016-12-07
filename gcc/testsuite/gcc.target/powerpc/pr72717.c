/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2" } */

typedef long V __attribute__((__vector_size__(32)));

extern void foo (V *, V*);

/* This test generated an failure in emit_move_insn.  */

void
foo(V *p, V *q)
{
  V v = *q;
  *p = v << v[0];
}

/* { dg-final { scan-assembler-times "vsld" 2 } } */
