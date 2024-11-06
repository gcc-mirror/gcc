/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2 -mrvv-max-lmul=m8" } */

#include "def.h"

void
spill_0 (int64_t *in, int64_t *out)
{
  v1di v1 = *(v1di*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v1di*)out = v1;
}

void
spill_1 (int64_t *in, int64_t *out)
{
  v2di v1 = *(v2di*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v2di*)out = v1;
}

void
spill_2 (int64_t *in, int64_t *out)
{
  v4di v1 = *(v4di*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v4di*)out = v1;
}

void
spill_3 (int64_t *in, int64_t *out)
{
  v8di v1 = *(v8di*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v8di*)out = v1;
}

void
spill_4 (int64_t *in, int64_t *out)
{
  v16di v1 = *(v16di*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v16di*)out = v1;
}

void
spill_5 (int64_t *in, int64_t *out)
{
  v32di v1 = *(v32di*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v32di*)out = v1;
}

void
spill_6 (int64_t *in, int64_t *out)
{
  v64di v1 = *(v64di*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v64di*)out = v1;
}

void
spill_7 (int64_t *in, int64_t *out)
{
  v128di v1 = *(v128di*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v128di*)out = v1;
}

void
spill_8 (int64_t *in, int64_t *out)
{
  v256di v1 = *(v256di*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v256di*)out = v1;
}

void
spill_9 (int64_t *in, int64_t *out)
{
  v512di v1 = *(v512di*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v512di*)out = v1;
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
/* { dg-final { scan-assembler-times {addi\tsp,sp,-2048} 3 } } */
/* { dg-final { scan-assembler-times {addi\tsp,sp,2032} 1 } } */
