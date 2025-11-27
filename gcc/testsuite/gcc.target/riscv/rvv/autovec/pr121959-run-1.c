/* { dg-do run } */
/* { dg-require-effective-target "riscv_zvbb_ok" } */
/* { dg-add-options "riscv_v" } */
/* { dg-add-options "riscv_zvbb" } */
/* { dg-additional-options "-std=c99" } */

#include "pr121959.h"

#define WT int32_t
#define NT uint8_t
#define IMM 16
#define N 16

DEF_VWSLL_FUNC_0_WRAP(WT, NT, IMM)

NT g_data[][2][N] = {
  {
    /* a */
    {
        2,   2,   2,   2,
      255, 255, 255, 255,
      128, 128, 128, 128,
      127, 127, 127, 127,
    },
    /* b */
    {
      1, 1, 1, 1,
      0, 0, 0, 0,
      2, 2, 2, 2,
      7, 7, 7, 7,
    },
  },
};

WT g_expect[][N] = {
  /* 0 */
  {
       65536,    65536,    65536,    65536,
    16711680, 16711680, 16711680, 16711680,
     8257536,  8257536,  8257536,  8257536,
     7864320,  7864320,  7864320,  7864320,
  },
};

int
main ()
{
  unsigned i, k;
  WT out[N];

  for (i = 0; i < sizeof (g_data) / sizeof (g_data[0]); i++)
    {
      NT *a = g_data[i][0];
      NT *b = g_data[i][1];
      WT *expect = g_expect[i];

      RUN_VWSLL_FUNC_0_WRAP (WT, NT, IMM, out, a, b, N);

      for (k = 0; k < N; k++)
	if (out[k] != expect[k])
	  __builtin_abort ();
    }

  return 0;
}
