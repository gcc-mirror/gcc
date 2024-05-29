/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

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
