/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -mrvv-max-lmul=m8 -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "def.h"

void
spill_0 (int16_t *in, int16_t *out)
{
  v1hi v1 = *(v1hi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v1hi*)out = v1;
}

void
spill_1 (int16_t *in, int16_t *out)
{
  v2hi v1 = *(v2hi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v2hi*)out = v1;
}

void
spill_2 (int16_t *in, int16_t *out)
{
  v4hi v1 = *(v4hi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v4hi*)out = v1;
}

void
spill_3 (int16_t *in, int16_t *out)
{
  v8hi v1 = *(v8hi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v8hi*)out = v1;
}

void
spill_4 (int16_t *in, int16_t *out)
{
  v16hi v1 = *(v16hi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v16hi*)out = v1;
}

void
spill_5 (int16_t *in, int16_t *out)
{
  v32hi v1 = *(v32hi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v32hi*)out = v1;
}

void
spill_6 (int16_t *in, int16_t *out)
{
  v64hi v1 = *(v64hi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v64hi*)out = v1;
}

void
spill_7 (int16_t *in, int16_t *out)
{
  v128hi v1 = *(v128hi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v128hi*)out = v1;
}

void
spill_8 (int16_t *in, int16_t *out)
{
  v256hi v1 = *(v256hi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v256hi*)out = v1;
}

void
spill_9 (int16_t *in, int16_t *out)
{
  v512hi v1 = *(v512hi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v512hi*)out = v1;
}

void
spill_10 (int16_t *in, int16_t *out)
{
  v1024hi v1 = *(v1024hi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v1024hi*)out = v1;
}

void
spill_11 (int16_t *in, int16_t *out)
{
  v2048hi v1 = *(v2048hi*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v2048hi*)out = v1;
}

/* { dg-final { scan-assembler-times {vle16\.v\tv[0-9]+,0\s*\([a-x0-9]+\)} 24 } } */
/* { dg-final { scan-assembler-times {vse16\.v\tv[0-9]+,0\s*\([a-x0-9]+\)} 24 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-16} 4 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-32} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-64} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-128} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-256} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-512} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-1024} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-2048} 1 } } */
/* { dg-final { scan-assembler-times {li\t[a-x0-9]+,-4096\s+add\tsp,sp,[a-x0-9]+} 1 } } */
