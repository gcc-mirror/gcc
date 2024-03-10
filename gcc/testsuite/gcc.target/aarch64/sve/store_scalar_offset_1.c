/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O3 -msve-vector-bits=256 --save-temps" } */

#include <stdint.h>

typedef int64_t vnx2di __attribute__((vector_size(32)));
typedef int32_t vnx4si __attribute__((vector_size(32)));
typedef int16_t vnx8hi __attribute__((vector_size(32)));
typedef int8_t vnx16qi __attribute__((vector_size(32)));

void sve_store_64_z_lsl (uint64_t *a, unsigned long i)
{
  asm volatile ("" : "=w" (*(vnx2di *) &a[i]));
}

void sve_store_64_s_lsl (int64_t *a, signed long i)
{
  asm volatile ("" : "=w" (*(vnx2di *) &a[i]));
}

void sve_store_32_z_lsl (uint32_t *a, unsigned long i)
{
  asm volatile ("" : "=w" (*(vnx4si *) &a[i]));
}

void sve_store_32_s_lsl (int32_t *a, signed long i)
{
  asm volatile ("" : "=w" (*(vnx4si *) &a[i]));
}

void sve_store_16_z_lsl (uint16_t *a, unsigned long i)
{
  asm volatile ("" : "=w" (*(vnx8hi *) &a[i]));
}

void sve_store_16_s_lsl (int16_t *a, signed long i)
{
  asm volatile ("" : "=w" (*(vnx8hi *) &a[i]));
}

/* ??? The other argument order leads to a redundant move.  */
void sve_store_8_z (unsigned long i, uint8_t *a)
{
  asm volatile ("" : "=w" (*(vnx16qi *) &a[i]));
}

void sve_store_8_s (signed long i, int8_t *a)
{
  asm volatile ("" : "=w" (*(vnx16qi *) &a[i]));
}

/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d, p[0-7], \[x0, x1, lsl 3\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, p[0-7], \[x0, x1, lsl 2\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.h, p[0-7], \[x0, x1, lsl 1\]\n} 2 } } */
/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.b, p[0-7], \[x1, x0\]\n} 2 } } */
