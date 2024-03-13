/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -mrvv-max-lmul=m8 -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "def.h"

void
spill_0 (float *in, float *out)
{
  v1sf v1 = *(v1sf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v1sf*)out = v1;
}

void
spill_1 (float *in, float *out)
{
  v2sf v1 = *(v2sf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v2sf*)out = v1;
}

void
spill_2 (float *in, float *out)
{
  v4sf v1 = *(v4sf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v4sf*)out = v1;
}

void
spill_3 (float *in, float *out)
{
  v8sf v1 = *(v8sf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v8sf*)out = v1;
}

void
spill_4 (float *in, float *out)
{
  v16sf v1 = *(v16sf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v16sf*)out = v1;
}

void
spill_5 (float *in, float *out)
{
  v32sf v1 = *(v32sf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v32sf*)out = v1;
}

void
spill_6 (float *in, float *out)
{
  v64sf v1 = *(v64sf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v64sf*)out = v1;
}

void
spill_7 (float *in, float *out)
{
  v128sf v1 = *(v128sf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v128sf*)out = v1;
}

void
spill_8 (float *in, float *out)
{
  v256sf v1 = *(v256sf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v256sf*)out = v1;
}

void
spill_9 (float *in, float *out)
{
  v512sf v1 = *(v512sf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v512sf*)out = v1;
}

void
spill_10 (float *in, float *out)
{
  v1024sf v1 = *(v1024sf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v1024sf*)out = v1;
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
