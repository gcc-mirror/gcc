/* { dg-do compile } */
/* { dg-options "-march=rv32gc -mabi=ilp32d -mtune=generic-ooo -mstringop-strategy=scalar" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Oz" "-Og" "-flto" } } */

#define MEMMOVE(N)				\
void movmem_##N (void *dst, void *src)	\
{						\
  __builtin_memmove (dst, src, N);		\
}

MEMMOVE(16)
MEMMOVE(32)
MEMMOVE(48)

/* { dg-final { scan-assembler-times "lw\t" 24 } } */
/* { dg-final { scan-assembler-times "sw\t" 24 } } */
/* { dg-final { scan-assembler-not "sb\t" } } */
/* { dg-final { scan-assembler-not "lbu\t" } } */
/* { dg-final { scan-assembler-not "memmove" } } */
