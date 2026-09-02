/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=generic-ooo -mstringop-strategy=scalar" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" "-Oz" "-Og" "-flto" } } */

#define MEMMOVE(N)				\
void movmem_##N (void *dst, void *src)	\
{						\
  __builtin_memmove (dst, src, N);		\
}

MEMMOVE(8)
MEMMOVE(16)
MEMMOVE(24)
MEMMOVE(32)

/* { dg-final { scan-assembler-times "ld\t" 10 } } */
/* { dg-final { scan-assembler-times "sd\t" 10 } } */
/* { dg-final { scan-assembler-not "sb\t" } } */
/* { dg-final { scan-assembler-not "lbu\t" } } */
/* { dg-final { scan-assembler-not "memmove" } } */
