/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -std=gnu11 -march=la64v1.1" } */

#pragma GCC push_options
#pragma GCC target ("no-frecipe")
float
frecipe (float src)
{
#ifdef __loongarch_frecipe
#error "Should't define __loongarch_frecipe"
#endif
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("no-div32")
int
div32 (unsigned long int a, unsigned long int b)
{
#ifdef __loongarch_div32
#error "Shouldn't define __loongarch_div32"
#endif
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("no-lam-bh")
short
lam_bh (short *ptr, short val)
{
#ifdef __loongarch_lam_bh
#error "Shouldn't define __loongarch_lam_bh"
#endif
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("no-lamcas")
void
lamcas (int *ptr, int *exp, int des)
{
#ifdef __loongarch_lamcas
#error "Shouldn't define __loongarch_lamcas"
#endif
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("no-scq")
void
scq (int *ptr, int *exp, int des)
{
#ifdef __loongarch_scq
#error "Shouldn't define __loongarch_scq"
#endif
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("no-ld-seq-sa")
void
ld_seq_sa (int *ptr, int *exp, int des)
{
#ifdef __loongarch_ld_seq_sa
#error "Shouldn't define __loongarch_ld_seq_sa"
#endif
}
#pragma GCC pop_options
