/* { dg-do compile { target int128 } } */
/* { dg-options "-O3 -march=skylake -mtune-ctrl=avx256_store_by_pieces" } */

extern __int128 array[16];

#define MK_CONST128_BROADCAST(A) \
  ((((unsigned __int128) (unsigned char) A) << 120) \
   | (((unsigned __int128) (unsigned char) A) << 112) \
   | (((unsigned __int128) (unsigned char) A) << 104) \
   | (((unsigned __int128) (unsigned char) A) << 96) \
   | (((unsigned __int128) (unsigned char) A) << 88) \
   | (((unsigned __int128) (unsigned char) A) << 80) \
   | (((unsigned __int128) (unsigned char) A) << 72) \
   | (((unsigned __int128) (unsigned char) A) << 64) \
   | (((unsigned __int128) (unsigned char) A) << 56) \
   | (((unsigned __int128) (unsigned char) A) << 48) \
   | (((unsigned __int128) (unsigned char) A) << 40) \
   | (((unsigned __int128) (unsigned char) A) << 32) \
   | (((unsigned __int128) (unsigned char) A) << 24) \
   | (((unsigned __int128) (unsigned char) A) << 16) \
   | (((unsigned __int128) (unsigned char) A) << 8) \
   | ((unsigned __int128) (unsigned char) A) )

void
foo (void)
{
  int i;
  for (i = 0; i < sizeof (array) / sizeof (array[0]); i++)
    array[i] = MK_CONST128_BROADCAST (0x1f);
}

/* { dg-final { scan-assembler-times "vpbroadcastb\[\\t \]+\[^\n\]*, %ymm\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu\[\\t \]%ymm\[0-9\]+, " 8 } } */
