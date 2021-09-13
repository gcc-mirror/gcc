/* { dg-do compile } */
/* Disable loop vectorization to avoid that loop vectorizer
   optimizes those two loops that operate tmp array so that
   subsequent dse3 won't eliminate expected tmp stores.  */
/* { dg-options "-O2 -funroll-loops -ftree-slp-vectorize -fno-tree-loop-vectorize -fdump-tree-dse-details" } */

/* Test if scalar cleanup pass takes effects, mainly check
   its secondary pass DSE can remove dead stores on array
   tmp.  */

#include "stdint.h"

static inline void
foo (int16_t *diff, int i_size, uint8_t *val1, int i_val1, uint8_t *val2,
     int i_val2)
{
  for (int y = 0; y < i_size; y++)
    {
      for (int x = 0; x < i_size; x++)
	diff[x + y * i_size] = val1[x] - val2[x];
      val1 += i_val1;
      val2 += i_val2;
    }
}

void
bar (int16_t res[16], uint8_t *val1, uint8_t *val2)
{
  int16_t d[16];
  int16_t tmp[16];

  foo (d, 4, val1, 16, val2, 32);

  for (int i = 0; i < 4; i++)
    {
      int s03 = d[i * 4 + 0] + d[i * 4 + 3];
      int s12 = d[i * 4 + 1] + d[i * 4 + 2];
      int d03 = d[i * 4 + 0] - d[i * 4 + 3];
      int d12 = d[i * 4 + 1] - d[i * 4 + 2];

      tmp[0 * 4 + i] = s03 + s12;
      tmp[1 * 4 + i] = 2 * d03 + d12;
      tmp[2 * 4 + i] = s03 - s12;
      tmp[3 * 4 + i] = d03 - 2 * d12;
    }

  for (int i = 0; i < 4; i++)
    {
      int s03 = tmp[i * 4 + 0] + tmp[i * 4 + 3];
      int s12 = tmp[i * 4 + 1] + tmp[i * 4 + 2];
      int d03 = tmp[i * 4 + 0] - tmp[i * 4 + 3];
      int d12 = tmp[i * 4 + 1] - tmp[i * 4 + 2];

      res[i * 4 + 0] = s03 + s12;
      res[i * 4 + 1] = 2 * d03 + d12;
      res[i * 4 + 2] = s03 - s12;
      res[i * 4 + 3] = d03 - 2 * d12;
    }
}

/* { dg-final { scan-tree-dump {Deleted dead store:.*tmp} "dse4" } } */
