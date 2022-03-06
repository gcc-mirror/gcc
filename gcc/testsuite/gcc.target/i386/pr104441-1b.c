/* { dg-do run } */
/* { dg-options "-O3 -march=x86-64 -mvzeroupper -Wno-attributes" } */

#include "pr104441-1a.c"

#define ARRAY_SIZE 255

__attribute__ ((noinline, noipa))
static void
do_test (void)
{
  uint8_t src[ARRAY_SIZE];
  uint8_t ref[ARRAY_SIZE];
  uint32_t x;
  uint32_t i;
  for (i = 0; i < ARRAY_SIZE; i++)
    {
      src[i] = i;
      ref[i] = i;
    }
  x = compute4x_m_sad_avx2_intrin(src, 64 >> 2, ref, 64, 4);
  if (x != 0x240)
    __builtin_abort ();
}

int
main ()
{
  if (__builtin_cpu_supports ("avx2"))
    do_test ();
  return 0;
}
