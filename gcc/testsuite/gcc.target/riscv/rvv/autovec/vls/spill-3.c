/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -mrvv-max-lmul=m8 -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "def.h"

void
spill_0 (int32_t *in, int32_t *out)
{
  v1si v1 = *(v1si*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v1si*)out = v1;
}

void
spill_1 (int32_t *in, int32_t *out)
{
  v2si v1 = *(v2si*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v2si*)out = v1;
}

void
spill_2 (int32_t *in, int32_t *out)
{
  v4si v1 = *(v4si*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v4si*)out = v1;
}

void
spill_3 (int32_t *in, int32_t *out)
{
  v8si v1 = *(v8si*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v8si*)out = v1;
}

void
spill_4 (int32_t *in, int32_t *out)
{
  v16si v1 = *(v16si*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v16si*)out = v1;
}

void
spill_5 (int32_t *in, int32_t *out)
{
  v32si v1 = *(v32si*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v32si*)out = v1;
}

void
spill_6 (int32_t *in, int32_t *out)
{
  v64si v1 = *(v64si*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v64si*)out = v1;
}

void
spill_7 (int32_t *in, int32_t *out)
{
  v128si v1 = *(v128si*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v128si*)out = v1;
}

void
spill_8 (int32_t *in, int32_t *out)
{
  v256si v1 = *(v256si*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v256si*)out = v1;
}

void
spill_9 (int32_t *in, int32_t *out)
{
  v512si v1 = *(v512si*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v512si*)out = v1;
}

void
spill_10 (int32_t *in, int32_t *out)
{
  v1024si v1 = *(v1024si*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v1024si*)out = v1;
}

/* { dg-final { scan-assembler-times {vle32\.v\tv[0-9]+,0\s*\([a-x0-9]+\)} 22 } } */
/* { dg-final { scan-assembler-times {vse32\.v\tv[0-9]+,0\s*\([a-x0-9]+\)} 22 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-16} 3 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-32} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-64} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-128} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-256} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-512} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-1024} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-2048} 1 } } */
/* { dg-final { scan-assembler-times {li\t[a-x0-9]+,-4096\s+add\tsp,sp,[a-x0-9]+} 1 } } */
