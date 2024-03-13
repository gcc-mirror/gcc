/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zvl4096b -mabi=lp64d -mrvv-max-lmul=m8 -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "def.h"

void
spill_0 (_Float16 *in, _Float16 *out)
{
  v1hf v1 = *(v1hf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v1hf*)out = v1;
}

void
spill_1 (_Float16 *in, _Float16 *out)
{
  v2hf v1 = *(v2hf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v2hf*)out = v1;
}

void
spill_2 (_Float16 *in, _Float16 *out)
{
  v4hf v1 = *(v4hf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v4hf*)out = v1;
}

void
spill_3 (_Float16 *in, _Float16 *out)
{
  v8hf v1 = *(v8hf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v8hf*)out = v1;
}

void
spill_4 (_Float16 *in, _Float16 *out)
{
  v16hf v1 = *(v16hf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v16hf*)out = v1;
}

void
spill_5 (_Float16 *in, _Float16 *out)
{
  v32hf v1 = *(v32hf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v32hf*)out = v1;
}

void
spill_6 (_Float16 *in, _Float16 *out)
{
  v64hf v1 = *(v64hf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v64hf*)out = v1;
}

void
spill_7 (_Float16 *in, _Float16 *out)
{
  v128hf v1 = *(v128hf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v128hf*)out = v1;
}

void
spill_8 (_Float16 *in, _Float16 *out)
{
  v256hf v1 = *(v256hf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v256hf*)out = v1;
}

void
spill_9 (_Float16 *in, _Float16 *out)
{
  v512hf v1 = *(v512hf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v512hf*)out = v1;
}

void
spill_10 (_Float16 *in, _Float16 *out)
{
  v1024hf v1 = *(v1024hf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v1024hf*)out = v1;
}

void
spill_11 (_Float16 *in, _Float16 *out)
{
  v2048hf v1 = *(v2048hf*)in;
  exhaust_vector_regs ();
  asm volatile ("" ::: "memory");
  *(v2048hf*)out = v1;
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
