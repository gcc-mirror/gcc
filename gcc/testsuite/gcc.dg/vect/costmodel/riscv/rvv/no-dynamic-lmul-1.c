/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl4096b -mabi=lp64d -O3 -ftree-vectorize -mrvv-max-lmul=dynamic -fdump-tree-vect-details" } */

#include <stdint-gcc.h>

void
foo (int8_t *restrict a)
{
  for (int i = 0; i < 4096; ++i)
    a[i] = a[i]-16;
}

void
foo2 (int16_t *restrict a)
{
  for (int i = 0; i < 2048; ++i)
    a[i] = a[i]-16;
}

void
foo3 (int32_t *restrict a)
{
  for (int i = 0; i < 1024; ++i)
    a[i] = a[i]-16;
}

void
foo4 (int64_t *restrict a)
{
  for (int i = 0; i < 512; ++i)
    a[i] = a[i]-16;
}

void
foo5 (int8_t *restrict a)
{
  for (int i = 0; i < 16; ++i)
    a[i] = a[i]-16;
}

void
foo6 (int16_t *restrict a)
{
  for (int i = 0; i < 16; ++i)
    a[i] = a[i]-16;
}

void
foo7 (int32_t *restrict a)
{
  for (int i = 0; i < 16; ++i)
    a[i] = a[i]-16;
}

void
foo8 (int64_t *restrict a)
{
  for (int i = 0; i < 16; ++i)
    a[i] = a[i]-16;
}

/* { dg-final { scan-tree-dump-not "Preferring smaller LMUL loop because it has unexpected spills" "vect" } } */
/* { dg-final { scan-assembler-times {vsetvli} 4 } } */
/* { dg-final { scan-assembler-times {vsetivli} 4 } } */
