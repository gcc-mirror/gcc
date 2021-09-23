/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power10 -O3 -dp" } */

#include <altivec.h>
#include <stdint.h>

#define ADD(a,b) ((a)+(b))
#define SUB1(a,b) ((a)-(b))
#define SUB2(a,b) ((b)-(a))

/* and/andc/eqv/nand/nor/or/orc/xor */
#define AND(a,b) ((a)&(b))
#define NAND(a,b) (~((a)&(b)))
#define NOR(a,b) (~((a)|(b)))
#define OR(a,b) ((a)|(b))
#define TEST1(type, func)							\
  type func ## _add_T_     ## type (type a, type b, type c) { return ADD(func(a,b),c); } \
  type func ## _sub1_T_    ## type (type a, type b, type c) { return SUB1(func(a,b),c); } \
  type func ## _sub2_T_    ## type (type a, type b, type c) { return SUB2(func(a,b),c); } \
  type func ## _rev_add_T_     ## type (type a, type b, type c) { return ADD(c,func(a,b)); } \
  type func ## _rev_sub1_T_    ## type (type a, type b, type c) { return SUB1(c,func(a,b)); } \
  type func ## _rev_sub2_T_    ## type (type a, type b, type c) { return SUB2(c,func(a,b)); }
#define TEST2(type, func)							\
  type func ## _and_T_     ## type (type a, type b, type c) { return  AND(func(a,b),c); } \
  type func ## _nand_T_    ## type (type a, type b, type c) { return NAND(func(a,b),c); } \
  type func ## _or_T_      ## type (type a, type b, type c) { return   OR(func(a,b),c); } \
  type func ## _nor_T_     ## type (type a, type b, type c) { return  NOR(func(a,b),c); } \
  type func ## _rev_and_T_     ## type (type a, type b, type c) { return  AND(c,func(a,b)); } \
  type func ## _rev_nand_T_    ## type (type a, type b, type c) { return NAND(c,func(a,b)); } \
  type func ## _rev_or_T_      ## type (type a, type b, type c) { return   OR(c,func(a,b)); } \
  type func ## _rev_nor_T_     ## type (type a, type b, type c) { return  NOR(c,func(a,b)); }
#define TEST(type)    \
  TEST1(type,AND)     \
  TEST1(type,NAND)    \
  TEST1(type,NOR)     \
  TEST1(type,OR)      \
  TEST2(type,ADD)     \
  TEST2(type,SUB1)    \
  TEST2(type,SUB2)

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

/* { dg-final { scan-assembler-times {\mfuse_add_and\M/}                           16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_add_nand\M/}                          16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_add_nor\M/}                           16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_add_or\M/}                            16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_and_add\M/}                           28 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_and_rsubf\M/}                         16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_and_subf\M/}                          16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nand_add\M/}                          16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nand_rsubf\M/}                         4 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nand_subf\M/}                         16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nor_add\M/}                           16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nor_rsubf\M/}                          4 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nor_subf\M/}                          16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_or_add\M/}                            28 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_or_rsubf\M/}                          16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_or_subf\M/}                           16 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_subf_and\M/}                          32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_subf_nand\M/}                         32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_subf_nor\M/}                          32 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mfuse_subf_or\M/}                           32 { target lp64 } } } */

/* { dg-final { scan-assembler-times {\mfuse_add_and\M/}                           12 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_add_nand\M/}                          12 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_add_nor\M/}                           12 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_add_or\M/}                            12 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_and_add\M/}                           22 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_and_rsubf\M/}                         12 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_and_subf\M/}                          12 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nand_add\M/}                          12 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nand_rsubf\M/}                         2 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nand_subf\M/}                         12 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nor_add\M/}                           12 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nor_rsubf\M/}                          2 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_nor_subf\M/}                          12 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_or_add\M/}                            22 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_or_rsubf\M/}                          12 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_or_subf\M/}                           12 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_subf_and\M/}                          24 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_subf_nand\M/}                         24 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_subf_nor\M/}                          24 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mfuse_subf_or\M/}                           24 { target ilp32 } } } */
