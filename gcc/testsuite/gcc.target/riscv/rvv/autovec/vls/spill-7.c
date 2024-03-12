/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "def.h"

void
spill_0 (int64_t *in, int64_t *out)
{
  v1df v1 = *(v1df*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v1df*)out = v1;
}

void
spill_1 (int64_t *in, int64_t *out)
{
  v2df v1 = *(v2df*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v2df*)out = v1;
}

void
spill_2 (int64_t *in, int64_t *out)
{
  v4df v1 = *(v4df*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v4df*)out = v1;
}

void
spill_3 (int64_t *in, int64_t *out)
{
  v8df v1 = *(v8df*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v8df*)out = v1;
}

void
spill_4 (int64_t *in, int64_t *out)
{
  v16df v1 = *(v16df*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v16df*)out = v1;
}

void
spill_5 (int64_t *in, int64_t *out)
{
  v32df v1 = *(v32df*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v32df*)out = v1;
}

void
spill_6 (int64_t *in, int64_t *out)
{
  v64df v1 = *(v64df*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v64df*)out = v1;
}

void
spill_7 (int64_t *in, int64_t *out)
{
  v128df v1 = *(v128df*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v128df*)out = v1;
}

void
spill_8 (int64_t *in, int64_t *out)
{
  v256df v1 = *(v256df*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v256df*)out = v1;
}

void
spill_9 (int64_t *in, int64_t *out)
{
  v512df v1 = *(v512df*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v512df*)out = v1;
}

/* { dg-final { scan-assembler-times {vle64\.v\tv[0-9]+,0\s*\([a-x0-9]+\)} 20 } } */
/* { dg-final { scan-assembler-times {vse64\.v\tv[0-9]+,0\s*\([a-x0-9]+\)} 20 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-16} 2 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-32} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-64} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-128} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-256} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-512} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-1024} 1 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,-2048} 1 } } */
/* { dg-final { scan-assembler-times {li\t[a-x0-9]+,-4096\s+add\tsp,sp,[a-x0-9]+} 1 } } */
