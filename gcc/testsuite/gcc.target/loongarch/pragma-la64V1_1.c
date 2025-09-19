/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -std=gnu11" } */
/* { dg-final { check-function-bodies "**" "" } } */


/*
** frecipe:
** 	frecipe.s	\$f0,\$f0
** 	jr	\$r1
*/
#pragma GCC push_options
#pragma GCC target ("frecipe")
float
frecipe (float src)
{
#ifndef __loongarch_frecipe
#error "Not define __loongarch_frecipe"
#endif
  return __builtin_loongarch_frecipe_s (src);
}
#pragma GCC pop_options


/*
** div32:
** 	div.w	\$r4,\$r4,\$r5
** 	jr	\$r1
*/
#pragma GCC push_options
#pragma GCC target ("div32")
int
div32 (unsigned long int a, unsigned long int b)
{
#ifndef __loongarch_div32
#error "Not define __loongarch_div32"
#endif
  return (int)a / (int)b;
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("lam-bh")
short
lam_bh (short *ptr, short val)
{
#ifndef __loongarch_lam_bh
#error "Not define __loongarch_lam_bh"
#endif
  return __atomic_fetch_add (ptr, val, __ATOMIC_RELAXED);
}
#pragma GCC pop_options
/* { dg-final { scan-assembler "lam_bh:.*amadd.h.*lam_bh" } } */

#pragma GCC push_options
#pragma GCC target ("lamcas")
void
lamcas (int *ptr, int *exp, int des)
{
#ifndef __loongarch_lamcas
#error "Not define __loongarch_lamcas"
#endif
  __atomic_compare_exchange_n (ptr, exp, des, 0, __ATOMIC_ACQ_REL, __ATOMIC_RELAXED);
}
#pragma GCC pop_options
/* { dg-final { scan-assembler "lamcas:.*amcas_db.w.*lamcas" } } */

#pragma GCC push_options
#pragma GCC target ("scq")
void
scq (int *ptr, int *exp, int des)
{
#ifndef __loongarch_scq
#error "Not define __loongarch_scq"
#endif
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("ld-seq-sa")
void
ld_seq_sa (int *ptr, int *exp, int des)
{
#ifndef __loongarch_ld_seq_sa
#error "Not define __loongarch_ld_seq_sa"
#endif
}
#pragma GCC pop_options
