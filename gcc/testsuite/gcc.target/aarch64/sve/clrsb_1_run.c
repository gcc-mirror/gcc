/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include "clrsb_1.c"

extern void abort (void) __attribute__ ((noreturn));

unsigned int data[] = {
  0xffffff80, 24,
  0xffffffff, 31,
  0x00000000, 31,
  0x80000000, 0,
  0x7fffffff, 0,
  0x000003ff, 21,
  0x1fffffff, 2,
  0x0000ffff, 15,
  0xffff0000, 15
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
  clrsb_32 (out32, in32, count);
  for (unsigned int i = 0; i < count; ++i)
    if (out32[i] != data[i * 2 + 1])
      abort ();

  uint64_t in64[count];
  unsigned int out64[count];
  for (unsigned int i = 0; i < count; ++i)
    {
      in64[i] = (uint64_t) data[i * 2] << 32;
      asm volatile ("" ::: "memory");
    }
  clrsb_64 (out64, in64, count);
  for (unsigned int i = 0; i < count; ++i)
    if (out64[i] != (data[i * 2] ? data[i * 2 + 1] : 63))
      abort ();

  return 0;
}
