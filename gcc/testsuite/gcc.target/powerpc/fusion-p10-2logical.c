/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-mdejagnu-cpu=power10 -O3 -dp" } */

#include <altivec.h>
#include <stdint.h>

/* and/andc/eqv/nand/nor/or/orc/xor */
#define AND(a,b) ((a)&(b))
#define ANDC1(a,b) ((a)&((~b)))
#define ANDC2(a,b) ((~(a))&(b))
#define EQV(a,b) (~((a)^(b)))
#define NAND(a,b) (~((a)&(b)))
#define NOR(a,b) (~((a)|(b)))
#define OR(a,b) ((a)|(b))
#define ORC1(a,b) ((a)|((~b)))
#define ORC2(a,b) ((~(a))|(b))
#define XOR(a,b) ((a)^(b))
#define TEST1(type, func)							\
  type func ## _and_T_     ## type (type a, type b, type c) { return AND(func(a,b),c); } \
  type func ## _andc1_T_   ## type (type a, type b, type c) { return ANDC1(func(a,b),c); } \
  type func ## _andc2_T_   ## type (type a, type b, type c) { return ANDC2(func(a,b),c); } \
  type func ## _eqv_T_     ## type (type a, type b, type c) { return EQV(func(a,b),c); } \
  type func ## _nand_T_    ## type (type a, type b, type c) { return NAND(func(a,b),c); } \
  type func ## _nor_T_     ## type (type a, type b, type c) { return NOR(func(a,b),c); } \
  type func ## _or_T_      ## type (type a, type b, type c) { return OR(func(a,b),c); } \
  type func ## _orc1_T_    ## type (type a, type b, type c) { return ORC1(func(a,b),c); } \
  type func ## _orc2_T_    ## type (type a, type b, type c) { return ORC2(func(a,b),c); } \
  type func ## _xor_T_     ## type (type a, type b, type c) { return XOR(func(a,b),c); } \
  type func ## _rev_and_T_     ## type (type a, type b, type c) { return AND(c,func(a,b)); } \
  type func ## _rev_andc1_T_   ## type (type a, type b, type c) { return ANDC1(c,func(a,b)); } \
  type func ## _rev_andc2_T_   ## type (type a, type b, type c) { return ANDC2(c,func(a,b)); } \
  type func ## _rev_eqv_T_     ## type (type a, type b, type c) { return EQV(c,func(a,b)); } \
  type func ## _rev_nand_T_    ## type (type a, type b, type c) { return NAND(c,func(a,b)); } \
  type func ## _rev_nor_T_     ## type (type a, type b, type c) { return NOR(c,func(a,b)); } \
  type func ## _rev_or_T_      ## type (type a, type b, type c) { return OR(c,func(a,b)); } \
  type func ## _rev_orc1_T_    ## type (type a, type b, type c) { return ORC1(c,func(a,b)); } \
  type func ## _rev_orc2_T_    ## type (type a, type b, type c) { return ORC2(c,func(a,b)); } \
  type func ## _rev_xor_T_     ## type (type a, type b, type c) { return XOR(c,func(a,b)); }
#define TEST(type)    \
  TEST1(type,AND)     \
  TEST1(type,ANDC1)   \
  TEST1(type,ANDC2)   \
  TEST1(type,EQV)     \
  TEST1(type,NAND)    \
  TEST1(type,NOR)     \
  TEST1(type,OR)      \
  TEST1(type,ORC1)    \
  TEST1(type,ORC2)    \
  TEST1(type,XOR)

typedef vector bool char vboolchar_t;
typedef vector unsigned int vuint_t;

TEST(uint8_t);
TEST(int8_t);
TEST(uint16_t);
TEST(int16_t);
TEST(uint32_t);
TEST(int32_t);
TEST(uint64_t);
TEST(int64_t);
TEST(vboolchar_t);
TEST(vuint_t);

/* Recreate with:
   grep ' \*fuse_' fusion-p10-2logical.s|sed -e 's,^.*\*,,' -e 's,/[0-9],/,' |sort -k 7,7 |uniq -c|awk '{l=30-length($2); printf("/%s* { %s { scan-assembler-times {\\m%s\\M}%-*s        %4d { target lp64 } } } *%s/\n","","dg-final",$2,l,"",$1,"");}'
 */

/* { dg-final { scan-assembler-times {\mfuse_and_and\M/}                           32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_andc_and\M/}                          96 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_andc_or\M/}                           64 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_andc_orc\M/}                          64 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_andc_xor\M/}                          64 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_and_eqv\M/}                           32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_and_or\M/}                            32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_and_orc\M/}                           32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_and_xor\M/}                           32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_eqv_and\M/}                           32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_eqv_andc\M/}                          32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_eqv_or\M/}                            32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nand_and\M/}                          32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nand_andc\M/}                         32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nand_or\M/}                           96 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nand_orc\M/}                          32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nor_and\M/}                           96 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nor_andc\M/}                          32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nor_or\M/}                            32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nor_orc\M/}                           32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_or_and\M/}                            32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_or_andc\M/}                           32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_orc_and\M/}                           64 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_orc_andc\M/}                          64 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_orc_or\M/}                            96 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_orc_xor\M/}                           64 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_or_eqv\M/}                            32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_or_or\M/}                             32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_or_xor\M/}                            32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vandc_vand\M/}                        24 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vandc_vor\M/}                         16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vandc_vorc\M/}                        16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vandc_vxor\M/}                        16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vand_vand\M/}                          8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vand_veqv\M/}                          8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vand_vor\M/}                           8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vand_vorc\M/}                          8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vand_vxor\M/}                          8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_veqv_vand\M/}                          8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_veqv_vandc\M/}                         8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_veqv_vor\M/}                           8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vnand_vand\M/}                         8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vnand_vandc\M/}                        8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vnand_vor\M/}                         24 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vnand_vorc\M/}                         8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vnor_vand\M/}                         24 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vnor_vandc\M/}                         8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vnor_vor\M/}                           8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vnor_vorc\M/}                          8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vorc_vand\M/}                         16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vorc_vandc\M/}                        16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vorc_vor\M/}                          24 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vorc_vxor\M/}                         16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vor_vand\M/}                           8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vor_vandc\M/}                          8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vor_veqv\M/}                           8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vor_vor\M/}                            8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vor_vxor\M/}                           8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vxor_vand\M/}                          8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vxor_vandc\M/}                         8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vxor_veqv\M/}                          8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vxor_vnand\M/}                         8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vxor_vor\M/}                           8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vxor_vorc\M/}                          8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vxor_vxor\M/}                          8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_xor_and\M/}                           32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_xor_andc\M/}                          32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_xor_eqv\M/}                           32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_xor_nand\M/}                          32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_xor_or\M/}                            32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_xor_orc\M/}                           32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_xor_xor\M/}                           32 { target lp64 } } } */

/* { dg-final { scan-assembler-times {\mfuse_and_and\M/}                           40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_andc_and\M/}                         120 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_andc_or\M/}                           80 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_andc_orc\M/}                          80 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_andc_xor\M/}                          80 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_and_eqv\M/}                           40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_and_or\M/}                            40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_and_orc\M/}                           40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_and_xor\M/}                           40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_eqv_and\M/}                           40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_eqv_andc\M/}                          40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_eqv_or\M/}                            40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nand_and\M/}                          40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nand_andc\M/}                         40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nand_or\M/}                          120 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nand_orc\M/}                          40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nor_and\M/}                          120 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nor_andc\M/}                          40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nor_or\M/}                            40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nor_orc\M/}                           40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_or_and\M/}                            40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_or_andc\M/}                           40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_orc_and\M/}                           80 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_orc_andc\M/}                          80 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_orc_or\M/}                           120 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_orc_xor\M/}                           80 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_or_eqv\M/}                            40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_or_or\M/}                             40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_or_xor\M/}                            40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vandc_vand\M/}                        24 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vandc_vor\M/}                         16 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vandc_vorc\M/}                        16 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vandc_vxor\M/}                        16 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vand_vand\M/}                          8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vand_veqv\M/}                          8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vand_vor\M/}                           8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vand_vorc\M/}                          8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vand_vxor\M/}                          8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_veqv_vand\M/}                          8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_veqv_vandc\M/}                         8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_veqv_vor\M/}                           8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vnand_vand\M/}                         8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vnand_vandc\M/}                        8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vnand_vor\M/}                         24 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vnand_vorc\M/}                         8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vnor_vand\M/}                         24 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vnor_vandc\M/}                         8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vnor_vor\M/}                           8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vnor_vorc\M/}                          8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vorc_vand\M/}                         16 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vorc_vandc\M/}                        16 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vorc_vor\M/}                          24 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vorc_vxor\M/}                         16 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vor_vand\M/}                           8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vor_vandc\M/}                          8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vor_veqv\M/}                           8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vor_vor\M/}                            8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vor_vxor\M/}                           8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vxor_vand\M/}                          8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vxor_vandc\M/}                         8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vxor_veqv\M/}                          8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vxor_vnand\M/}                         8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vxor_vor\M/}                           8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vxor_vorc\M/}                          8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_vxor_vxor\M/}                          8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_xor_and\M/}                           40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_xor_andc\M/}                          40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_xor_eqv\M/}                           40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_xor_nand\M/}                          40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_xor_or\M/}                            40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_xor_orc\M/}                           40 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_xor_xor\M/}                           40 { target ilp32 } } } */
