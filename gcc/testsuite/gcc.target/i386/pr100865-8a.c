/* { dg-do compile { target int128 } } */
/* { dg-options "-O3 -march=skylake" } */

extern __int128 array[16];

#define MK_CONST128_BROADCAST(A) \
  ((((unsigned __int128) (unsigned int) A) << 96) \
   | (((unsigned __int128) (unsigned int) A) << 64) \
   | (((unsigned __int128) (unsigned int) A) << 32) \
   | ((unsigned __int128) (unsigned int) A) )

#define MK_CONST128_BROADCAST_SIGNED(A) \
  ((__int128) MK_CONST128_BROADCAST (A))

void
foo (void)
{
  int i;
  for (i = 0; i < sizeof (array) / sizeof (array[0]); i++)
    array[i] = MK_CONST128_BROADCAST_SIGNED (-45);
}

/* { dg-final { scan-assembler-times "(?:vpbroadcastd|vpshufd)\[\\t \]+\[^\n\]*, %xmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa\[\\t \]%xmm\[0-9\]+, " 16 } } */
