/* { dg-do compile } */
/* { dg-options "-march=rv64imfd_zalrsc -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } {"-O0"} } */

/* lr.w/sc.w */
int *i;
int lr_sc(int v)
{
  return __atomic_exchange_4(i, v, __ATOMIC_RELAXED);
}

/* { dg-final { scan-assembler-times {\mlr.w} 1 } } */
/* { dg-final { scan-assembler-times {\msc.w} 1 } } */
/* { dg-final { scan-assembler-not   {"mv\t"}   } } */
