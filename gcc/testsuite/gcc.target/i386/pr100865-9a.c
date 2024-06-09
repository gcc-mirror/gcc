/* { dg-do compile { target int128 } } */
/* { dg-options "-O3 -march=skylake" } */

extern __int128 array[16];

#define MK_CONST128_BROADCAST(A) \
  ((((unsigned __int128) (unsigned short) A) << 112) \
   | (((unsigned __int128) (unsigned short) A) << 96) \
   | (((unsigned __int128) (unsigned short) A) << 80) \
   | (((unsigned __int128) (unsigned short) A) << 64) \
   | (((unsigned __int128) (unsigned short) A) << 48) \
   | (((unsigned __int128) (unsigned short) A) << 32) \
   | (((unsigned __int128) (unsigned short) A) << 16) \
   | ((unsigned __int128) (unsigned short) A) )

void
foo (void)
{
  int i;
  for (i = 0; i < sizeof (array) / sizeof (array[0]); i++)
    array[i] = MK_CONST128_BROADCAST (0x1234);
}

/* { dg-final { scan-assembler-times "vpbroadcastd\[\\t \]+%xmm\[0-9\]+, %xmm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa\[\\t \]%xmm\[0-9\]+, " 16 } } */
