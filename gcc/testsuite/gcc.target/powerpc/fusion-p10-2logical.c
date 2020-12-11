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
   grep ' \*fuse_' fusion-p10-2logical.s|sed -e 's,^.*\*,,' |sort -k 7,7 |uniq -c|awk '{l=30-length($2); printf("/%s* { %s { scan-assembler-times \"%s\"%-*s        %4d } } *%s/\n","","dg-final",$2,l,"",$1,"");}'
 */
  
/* { dg-final { scan-assembler-times "fuse_and_and/1"                          16 } } */
/* { dg-final { scan-assembler-times "fuse_and_and/2"                          16 } } */
/* { dg-final { scan-assembler-times "fuse_andc_and/0"                         16 } } */
/* { dg-final { scan-assembler-times "fuse_andc_and/1"                         26 } } */
/* { dg-final { scan-assembler-times "fuse_andc_and/2"                         48 } } */
/* { dg-final { scan-assembler-times "fuse_andc_and/3"                          6 } } */
/* { dg-final { scan-assembler-times "fuse_andc_or/0"                          16 } } */
/* { dg-final { scan-assembler-times "fuse_andc_or/1"                          16 } } */
/* { dg-final { scan-assembler-times "fuse_andc_or/2"                          32 } } */
/* { dg-final { scan-assembler-times "fuse_andc_orc/0"                          8 } } */
/* { dg-final { scan-assembler-times "fuse_andc_orc/1"                          8 } } */
/* { dg-final { scan-assembler-times "fuse_andc_orc/2"                         48 } } */
/* { dg-final { scan-assembler-times "fuse_andc_xor/0"                         16 } } */
/* { dg-final { scan-assembler-times "fuse_andc_xor/1"                         16 } } */
/* { dg-final { scan-assembler-times "fuse_andc_xor/2"                         32 } } */
/* { dg-final { scan-assembler-times "fuse_and_eqv/0"                           8 } } */
/* { dg-final { scan-assembler-times "fuse_and_eqv/2"                          24 } } */
/* { dg-final { scan-assembler-times "fuse_and_or/0"                           16 } } */
/* { dg-final { scan-assembler-times "fuse_and_or/2"                           16 } } */
/* { dg-final { scan-assembler-times "fuse_and_orc/0"                           8 } } */
/* { dg-final { scan-assembler-times "fuse_and_orc/2"                          24 } } */
/* { dg-final { scan-assembler-times "fuse_and_xor/0"                          16 } } */
/* { dg-final { scan-assembler-times "fuse_and_xor/2"                          16 } } */
/* { dg-final { scan-assembler-times "fuse_eqv_and/0"                          16 } } */
/* { dg-final { scan-assembler-times "fuse_eqv_and/2"                          16 } } */
/* { dg-final { scan-assembler-times "fuse_eqv_andc/0"                          8 } } */
/* { dg-final { scan-assembler-times "fuse_eqv_andc/2"                         24 } } */
/* { dg-final { scan-assembler-times "fuse_eqv_or/0"                            8 } } */
/* { dg-final { scan-assembler-times "fuse_eqv_or/2"                           24 } } */
/* { dg-final { scan-assembler-times "fuse_nand_and/0"                         16 } } */
/* { dg-final { scan-assembler-times "fuse_nand_and/2"                         16 } } */
/* { dg-final { scan-assembler-times "fuse_nand_andc/0"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_nand_andc/2"                        24 } } */
/* { dg-final { scan-assembler-times "fuse_nand_or/0"                          14 } } */
/* { dg-final { scan-assembler-times "fuse_nand_or/1"                           2 } } */
/* { dg-final { scan-assembler-times "fuse_nand_or/2"                          72 } } */
/* { dg-final { scan-assembler-times "fuse_nand_or/3"                           8 } } */
/* { dg-final { scan-assembler-times "fuse_nand_orc/2"                         24 } } */
/* { dg-final { scan-assembler-times "fuse_nand_orc/3"                          8 } } */
/* { dg-final { scan-assembler-times "fuse_nor_and/0"                          28 } } */
/* { dg-final { scan-assembler-times "fuse_nor_and/1"                           4 } } */
/* { dg-final { scan-assembler-times "fuse_nor_and/2"                          48 } } */
/* { dg-final { scan-assembler-times "fuse_nor_and/3"                          16 } } */
/* { dg-final { scan-assembler-times "fuse_nor_andc/2"                         24 } } */
/* { dg-final { scan-assembler-times "fuse_nor_andc/3"                          8 } } */
/* { dg-final { scan-assembler-times "fuse_nor_or/0"                            8 } } */
/* { dg-final { scan-assembler-times "fuse_nor_or/2"                           24 } } */
/* { dg-final { scan-assembler-times "fuse_nor_orc/0"                           8 } } */
/* { dg-final { scan-assembler-times "fuse_nor_orc/2"                          24 } } */
/* { dg-final { scan-assembler-times "fuse_or_and/0"                           16 } } */
/* { dg-final { scan-assembler-times "fuse_or_and/2"                           16 } } */
/* { dg-final { scan-assembler-times "fuse_or_andc/0"                          16 } } */
/* { dg-final { scan-assembler-times "fuse_or_andc/2"                          16 } } */
/* { dg-final { scan-assembler-times "fuse_orc_and/0"                          16 } } */
/* { dg-final { scan-assembler-times "fuse_orc_and/1"                          16 } } */
/* { dg-final { scan-assembler-times "fuse_orc_and/2"                          32 } } */
/* { dg-final { scan-assembler-times "fuse_orc_andc/0"                          8 } } */
/* { dg-final { scan-assembler-times "fuse_orc_andc/1"                          8 } } */
/* { dg-final { scan-assembler-times "fuse_orc_andc/2"                         48 } } */
/* { dg-final { scan-assembler-times "fuse_orc_or/0"                            8 } } */
/* { dg-final { scan-assembler-times "fuse_orc_or/1"                           10 } } */
/* { dg-final { scan-assembler-times "fuse_orc_or/2"                           72 } } */
/* { dg-final { scan-assembler-times "fuse_orc_or/3"                            6 } } */
/* { dg-final { scan-assembler-times "fuse_orc_xor/0"                           8 } } */
/* { dg-final { scan-assembler-times "fuse_orc_xor/1"                           8 } } */
/* { dg-final { scan-assembler-times "fuse_orc_xor/2"                          48 } } */
/* { dg-final { scan-assembler-times "fuse_or_eqv/0"                            8 } } */
/* { dg-final { scan-assembler-times "fuse_or_eqv/2"                           24 } } */
/* { dg-final { scan-assembler-times "fuse_or_or/1"                            16 } } */
/* { dg-final { scan-assembler-times "fuse_or_or/2"                            16 } } */
/* { dg-final { scan-assembler-times "fuse_or_xor/0"                           16 } } */
/* { dg-final { scan-assembler-times "fuse_or_xor/2"                           16 } } */
/* { dg-final { scan-assembler-times "fuse_vandc_vand/0"                        8 } } */
/* { dg-final { scan-assembler-times "fuse_vandc_vand/1"                       10 } } */
/* { dg-final { scan-assembler-times "fuse_vandc_vand/3"                        6 } } */
/* { dg-final { scan-assembler-times "fuse_vandc_vor/0"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_vandc_vor/1"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_vandc_vorc/0"                        8 } } */
/* { dg-final { scan-assembler-times "fuse_vandc_vorc/1"                        8 } } */
/* { dg-final { scan-assembler-times "fuse_vandc_vxor/0"                        8 } } */
/* { dg-final { scan-assembler-times "fuse_vandc_vxor/1"                        8 } } */
/* { dg-final { scan-assembler-times "fuse_vand_vand/1"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_vand_veqv/0"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_vand_vor/0"                          8 } } */
/* { dg-final { scan-assembler-times "fuse_vand_vorc/0"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_vand_vxor/0"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_veqv_vand/0"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_veqv_vandc/0"                        8 } } */
/* { dg-final { scan-assembler-times "fuse_veqv_vor/0"                          8 } } */
/* { dg-final { scan-assembler-times "fuse_vnand_vand/0"                        8 } } */
/* { dg-final { scan-assembler-times "fuse_vnand_vandc/0"                       8 } } */
/* { dg-final { scan-assembler-times "fuse_vnand_vor/0"                        14 } } */
/* { dg-final { scan-assembler-times "fuse_vnand_vor/1"                         2 } } */
/* { dg-final { scan-assembler-times "fuse_vnand_vor/3"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_vnand_vorc/3"                        8 } } */
/* { dg-final { scan-assembler-times "fuse_vnor_vand/0"                        14 } } */
/* { dg-final { scan-assembler-times "fuse_vnor_vand/1"                         2 } } */
/* { dg-final { scan-assembler-times "fuse_vnor_vand/3"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_vnor_vandc/3"                        8 } } */
/* { dg-final { scan-assembler-times "fuse_vnor_vor/0"                          8 } } */
/* { dg-final { scan-assembler-times "fuse_vnor_vorc/0"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_vorc_vand/0"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_vorc_vand/1"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_vorc_vandc/0"                        8 } } */
/* { dg-final { scan-assembler-times "fuse_vorc_vandc/1"                        8 } } */
/* { dg-final { scan-assembler-times "fuse_vorc_vor/0"                          8 } } */
/* { dg-final { scan-assembler-times "fuse_vorc_vor/1"                         10 } } */
/* { dg-final { scan-assembler-times "fuse_vorc_vor/3"                          6 } } */
/* { dg-final { scan-assembler-times "fuse_vorc_vxor/0"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_vorc_vxor/1"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_vor_vand/0"                          8 } } */
/* { dg-final { scan-assembler-times "fuse_vor_vandc/0"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_vor_veqv/0"                          8 } } */
/* { dg-final { scan-assembler-times "fuse_vor_vor/1"                           8 } } */
/* { dg-final { scan-assembler-times "fuse_vor_vxor/0"                          8 } } */
/* { dg-final { scan-assembler-times "fuse_vxor_vand/0"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_vxor_vandc/0"                        8 } } */
/* { dg-final { scan-assembler-times "fuse_vxor_veqv/3"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_vxor_vnand/0"                        8 } } */
/* { dg-final { scan-assembler-times "fuse_vxor_vor/0"                          8 } } */
/* { dg-final { scan-assembler-times "fuse_vxor_vorc/0"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_vxor_vxor/1"                         8 } } */
/* { dg-final { scan-assembler-times "fuse_xor_and/0"                          16 } } */
/* { dg-final { scan-assembler-times "fuse_xor_and/2"                          16 } } */
/* { dg-final { scan-assembler-times "fuse_xor_andc/0"                         16 } } */
/* { dg-final { scan-assembler-times "fuse_xor_andc/2"                         16 } } */
/* { dg-final { scan-assembler-times "fuse_xor_eqv/2"                          24 } } */
/* { dg-final { scan-assembler-times "fuse_xor_eqv/3"                           8 } } */
/* { dg-final { scan-assembler-times "fuse_xor_nand/0"                          8 } } */
/* { dg-final { scan-assembler-times "fuse_xor_nand/2"                         24 } } */
/* { dg-final { scan-assembler-times "fuse_xor_or/0"                           16 } } */
/* { dg-final { scan-assembler-times "fuse_xor_or/2"                           16 } } */
/* { dg-final { scan-assembler-times "fuse_xor_orc/0"                           8 } } */
/* { dg-final { scan-assembler-times "fuse_xor_orc/2"                          24 } } */
/* { dg-final { scan-assembler-times "fuse_xor_xor/1"                          16 } } */
/* { dg-final { scan-assembler-times "fuse_xor_xor/2"                          16 } } */
