/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -mrvv-max-lmul=m8 -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "def.h"

void
spill_0 (int8_t *in, int8_t *out)
{
  v1qi v1 = *(v1qi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v1qi*)out = v1;
}

void
spill_1 (int8_t *in, int8_t *out)
{
  v2qi v1 = *(v2qi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v2qi*)out = v1;
}

void
spill_2 (int8_t *in, int8_t *out)
{
  v4qi v1 = *(v4qi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v4qi*)out = v1;
}

void
spill_3 (int8_t *in, int8_t *out)
{
  v8qi v1 = *(v8qi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v8qi*)out = v1;
}

void
spill_4 (int8_t *in, int8_t *out)
{
  v16qi v1 = *(v16qi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v16qi*)out = v1;
}

void
spill_5 (int8_t *in, int8_t *out)
{
  v32qi v1 = *(v32qi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v32qi*)out = v1;
}

void
spill_6 (int8_t *in, int8_t *out)
{
  v64qi v1 = *(v64qi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v64qi*)out = v1;
}

void
spill_7 (int8_t *in, int8_t *out)
{
  v128qi v1 = *(v128qi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v128qi*)out = v1;
}

void
spill_8 (int8_t *in, int8_t *out)
{
  v256qi v1 = *(v256qi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v256qi*)out = v1;
}

void
spill_9 (int8_t *in, int8_t *out)
{
  v512qi v1 = *(v512qi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v512qi*)out = v1;
}

void
spill_10 (int8_t *in, int8_t *out)
{
  v1024qi v1 = *(v1024qi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v1024qi*)out = v1;
}

void
spill_11 (int8_t *in, int8_t *out)
{
  v2048qi v1 = *(v2048qi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v2048qi*)out = v1;
}

void
spill_12 (int8_t *in, int8_t *out)
{
  v4096qi v1 = *(v4096qi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v4096qi*)out = v1;
}

/* { dg-final { scan-assembler-times {vle8\.v\tv[0-9]+,0\s*\([a-x0-9]+\)} 26 } } */
/* { dg-final { scan-assembler-times {vse8\.v\tv[0-9]+,0\s*\([a-x0-9]+\)} 26 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-16} 5 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-32} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-64} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-128} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-256} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-512} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-1024} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-2048} 3 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,2032} 1 } } */
