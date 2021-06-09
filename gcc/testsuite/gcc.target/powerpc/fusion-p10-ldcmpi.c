/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-mdejagnu-cpu=power10 -O3 -dp" } */

#include <stdint.h>

#define TEST(type) \
  struct t ## type { type x[128],a,b,c; };		\
  struct s ## type { struct t ## type *p; int d,e,f; uint64_t g,h,i; };	\
int \
t ## type (struct s ## type *p) \
{ \
  struct t ## type *tp = p->p; \
  if(!tp || tp->a > ((type)0)) { return p->d; }	\
  if(!tp || tp->b > ((type)1)) { return p->e; }	\
  if(!tp || ( ((type)(-1) < 0) && tp->c < ((type)-1))) { return p->f; }	\
  return 0; \
} \
type \
t ## type ## _ret (struct s ## type *p) \
{ \
  struct t ## type *tp = p->p; \
  if(!tp || tp->a > ((type)0)) { return tp->a; }	\
  if(!tp || tp->b > ((type)1)) { return tp->b; }	\
  if(!tp || ( ((type)(-1) < 0) && tp->c < ((type)-1))) { return tp->c; }	\
  return 0; \
}\
int \
t ## type ## _x (struct s ## type *p) \
{ \
  struct t ## type *tp = p->p; \
  if(!tp || tp->x[p->g] > ((type)0)) { return p->d; }	\
  if(!tp || tp->x[p->h] > ((type)1)) { return p->e; }	\
  if(!tp || ( ((type)(-1) < 0) && tp->x[p->i] < ((type)-1))) { return p->f; }	\
  return 0; \
} \
type \
t ## type ## _x_ret (struct s ## type *p) \
{ \
  struct t ## type *tp = p->p; \
  if(!tp || tp->x[p->g] > ((type)0)) { return tp->x[p->g]; }	\
  if(!tp || tp->x[p->h] > ((type)1)) { return tp->x[p->h]; }	\
  if(!tp || ( ((type)(-1) < 0) && tp->x[p->i] < ((type)-1))) { return tp->x[p->i]; }	\
  return 0; \
}

TEST(uint64_t)
TEST(int64_t)
TEST(uint32_t)
TEST(int32_t)
TEST(uint16_t)
TEST(int16_t)
TEST(uint8_t)
TEST(int8_t)

/* { dg-final { scan-assembler-times "lbz_cmpldi_cr0_QI_clobber_CCUNS_zero"   2 { target lp64 } } } */
/* { dg-final { scan-assembler-times "ld_cmpdi_cr0_DI_DI_CC_none"             4 { target lp64 } } } */
/* { dg-final { scan-assembler-times "ld_cmpdi_cr0_DI_clobber_CC_none"        4 { target lp64 } } } */
/* { dg-final { scan-assembler-times "ld_cmpldi_cr0_DI_DI_CCUNS_none"         1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "ld_cmpldi_cr0_DI_clobber_CCUNS_none"    1 { target lp64 } } } */
/* { dg-final { scan-assembler-times "lha_cmpdi_cr0_HI_clobber_CC_sign"       8 { target lp64 } } } */
/* { dg-final { scan-assembler-times "lhz_cmpldi_cr0_HI_clobber_CCUNS_zero"   2 { target lp64 } } } */
/* { dg-final { scan-assembler-times "lwa_cmpdi_cr0_SI_EXTSI_CC_sign"         3 { target lp64 } } } */
/* { dg-final { scan-assembler-times "lwa_cmpdi_cr0_SI_clobber_CC_none"       4 { target lp64 } } } */
/* { dg-final { scan-assembler-times "lwz_cmpldi_cr0_SI_EXTSI_CCUNS_zero"     2 { target lp64 } } } */
/* { dg-final { scan-assembler-times "lwz_cmpldi_cr0_SI_clobber_CCUNS_none"   2 { target lp64 } } } */

/* { dg-final { scan-assembler-times "lbz_cmpldi_cr0_QI_clobber_CCUNS_zero"   2 { target ilp32 } } } */
/* { dg-final { scan-assembler-times "ld_cmpdi_cr0_DI_DI_CC_none"             0 { target ilp32 } } } */
/* { dg-final { scan-assembler-times "ld_cmpdi_cr0_DI_clobber_CC_none"        0 { target ilp32 } } } */
/* { dg-final { scan-assembler-times "ld_cmpldi_cr0_DI_DI_CCUNS_none"         0 { target ilp32 } } } */
/* { dg-final { scan-assembler-times "ld_cmpldi_cr0_DI_clobber_CCUNS_none"    0 { target ilp32 } } } */
/* { dg-final { scan-assembler-times "lha_cmpdi_cr0_HI_clobber_CC_sign"       8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times "lhz_cmpldi_cr0_HI_clobber_CCUNS_zero"   2 { target ilp32 } } } */
/* { dg-final { scan-assembler-times "lwa_cmpdi_cr0_SI_EXTSI_CC_sign"         0 { target ilp32 } } } */
/* { dg-final { scan-assembler-times "lwa_cmpdi_cr0_SI_clobber_CC_none"       9 { target ilp32 } } } */
/* { dg-final { scan-assembler-times "lwz_cmpldi_cr0_SI_EXTSI_CCUNS_zero"     0 { target ilp32 } } } */
/* { dg-final { scan-assembler-times "lwz_cmpldi_cr0_SI_clobber_CCUNS_none"   6 { target ilp32 } } } */
