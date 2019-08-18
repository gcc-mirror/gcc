/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "clz_1.c"

extern void abort (void) __attribute__ ((noreturn));

unsigned int data[] = {
  0xffffff80, 0,
  0xffffffff, 0,
  0x00000000, 32,
  0x80000000, 0,
  0x7fffffff, 1,
  0x000003ff, 22,
  0x1fffffff, 3,
  0x0000ffff, 16,
  0xffff0000, 0
};

int __attribute__ ((optimize (1)))
main (void)
{
  unsigned int count = sizeof (data) / sizeof (data[0]) / 2;

  uint32_t in32[count];
  unsigned int out32[count];
  for (unsigned int i = 0; i < count; ++i)
    {
      in32[i] = data[i * 2];
      asm volatile ("" ::: "memory");
    }
  clz_32 (out32, in32, count);
  for (unsigned int i = 0; i < count; ++i)
    if (out32[i] != data[i * 2 + 1])
      abort ();

  uint64_t in64[count];
  unsigned int out64[count];
  for (unsigned int i = 0; i < count; ++i)
    {
      in64[i] = (uint64_t) data[i * 2] << 10;
      asm volatile ("" ::: "memory");
    }
  clz_64 (out64, in64, count);
  for (unsigned int i = 0; i < count; ++i)
    if (out64[i] != (data[i * 2] ? data[i * 2 + 1] + 22 : 64))
      abort ();

  return 0;
}
