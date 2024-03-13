/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -mrvv-max-lmul=m8 -fdump-tree-optimized-details" } */

#include "def.h"

DEF_REDUC_BITWISE (int32_t, and, &=, 4)
DEF_REDUC_BITWISE (int32_t, and, &=, 8)
DEF_REDUC_BITWISE (int32_t, and, &=, 16)
DEF_REDUC_BITWISE (int32_t, and, &=, 32)
DEF_REDUC_BITWISE (int32_t, and, &=, 64)
DEF_REDUC_BITWISE (int32_t, and, &=, 128)
DEF_REDUC_BITWISE (int32_t, and, &=, 256)
DEF_REDUC_BITWISE (int32_t, and, &=, 512)
DEF_REDUC_BITWISE (int32_t, and, &=, 1024)

DEF_REDUC_BITWISE (uint32_t, and, &=, 4)
DEF_REDUC_BITWISE (uint32_t, and, &=, 8)
DEF_REDUC_BITWISE (uint32_t, and, &=, 16)
DEF_REDUC_BITWISE (uint32_t, and, &=, 32)
DEF_REDUC_BITWISE (uint32_t, and, &=, 64)
DEF_REDUC_BITWISE (uint32_t, and, &=, 128)
DEF_REDUC_BITWISE (uint32_t, and, &=, 256)
DEF_REDUC_BITWISE (uint32_t, and, &=, 512)
DEF_REDUC_BITWISE (uint32_t, and, &=, 1024)

DEF_REDUC_BITWISE (int32_t, ior, |=, 4)
DEF_REDUC_BITWISE (int32_t, ior, |=, 8)
DEF_REDUC_BITWISE (int32_t, ior, |=, 16)
DEF_REDUC_BITWISE (int32_t, ior, |=, 32)
DEF_REDUC_BITWISE (int32_t, ior, |=, 64)
DEF_REDUC_BITWISE (int32_t, ior, |=, 128)
DEF_REDUC_BITWISE (int32_t, ior, |=, 256)
DEF_REDUC_BITWISE (int32_t, ior, |=, 512)
DEF_REDUC_BITWISE (int32_t, ior, |=, 1024)

DEF_REDUC_BITWISE (uint32_t, ior, |=, 4)
DEF_REDUC_BITWISE (uint32_t, ior, |=, 8)
DEF_REDUC_BITWISE (uint32_t, ior, |=, 16)
DEF_REDUC_BITWISE (uint32_t, ior, |=, 32)
DEF_REDUC_BITWISE (uint32_t, ior, |=, 64)
DEF_REDUC_BITWISE (uint32_t, ior, |=, 128)
DEF_REDUC_BITWISE (uint32_t, ior, |=, 256)
DEF_REDUC_BITWISE (uint32_t, ior, |=, 512)
DEF_REDUC_BITWISE (uint32_t, ior, |=, 1024)

DEF_REDUC_BITWISE (int32_t, xor, ^=, 4)
DEF_REDUC_BITWISE (int32_t, xor, ^=, 8)
DEF_REDUC_BITWISE (int32_t, xor, ^=, 16)
DEF_REDUC_BITWISE (int32_t, xor, ^=, 32)
DEF_REDUC_BITWISE (int32_t, xor, ^=, 64)
DEF_REDUC_BITWISE (int32_t, xor, ^=, 128)
DEF_REDUC_BITWISE (int32_t, xor, ^=, 256)
DEF_REDUC_BITWISE (int32_t, xor, ^=, 512)
DEF_REDUC_BITWISE (int32_t, xor, ^=, 1024)

DEF_REDUC_BITWISE (uint32_t, xor, ^=, 4)
DEF_REDUC_BITWISE (uint32_t, xor, ^=, 8)
DEF_REDUC_BITWISE (uint32_t, xor, ^=, 16)
DEF_REDUC_BITWISE (uint32_t, xor, ^=, 32)
DEF_REDUC_BITWISE (uint32_t, xor, ^=, 64)
DEF_REDUC_BITWISE (uint32_t, xor, ^=, 128)
DEF_REDUC_BITWISE (uint32_t, xor, ^=, 256)
DEF_REDUC_BITWISE (uint32_t, xor, ^=, 512)
DEF_REDUC_BITWISE (uint32_t, xor, ^=, 1024)

/* { dg-final { scan-assembler-times {vredand\.vs} 18 } } */
/* { dg-final { scan-assembler-times {vredor\.vs} 18 } } */
/* { dg-final { scan-assembler-times {vredxor\.vs} 18 } } */
/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-tree-dump-not "1,1" "optimized" } } */
/* { dg-final { scan-tree-dump-not "2,2" "optimized" } } */
/* { dg-final { scan-tree-dump-not "4,4" "optimized" } } */
/* { dg-final { scan-tree-dump-not "16,16" "optimized" } } */
/* { dg-final { scan-tree-dump-not "32,32" "optimized" } } */
/* { dg-final { scan-tree-dump-not "64,64" "optimized" } } */
/* { dg-final { scan-tree-dump-not "128,128" "optimized" } } */
/* { dg-final { scan-tree-dump-not "256,256" "optimized" } } */
/* { dg-final { scan-tree-dump-not "512,512" "optimized" } } */
/* { dg-final { scan-tree-dump-not "1024,1024" "optimized" } } */
/* { dg-final { scan-tree-dump-not "2048,2048" "optimized" } } */
/* { dg-final { scan-tree-dump-not "4096,4096" "optimized" } } */
