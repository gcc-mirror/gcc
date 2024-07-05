#include "tree-vect.h"

/* This is a load-lane / masked-store-lane test that more reliably
   triggers SLP than SVEs mask_srtuct_store_*.c  */

void __attribute__ ((noipa))
test4 (int *__restrict dest, int *__restrict src,
       int *__restrict cond, int bias, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int value0 = src[i * 4] + bias;
      int value1 = src[i * 4 + 1] * bias;
      int value2 = src[i * 4 + 2] + bias;
      int value3 = src[i * 4 + 3] * bias;
      if (cond[i])
        {
          dest[i * 4] = value0;
          dest[i * 4 + 1] = value1;
          dest[i * 4 + 2] = value2;
          dest[i * 4 + 3] = value3;
        }
    }
}

int dest[16*4];
int src[16*4];
int cond[16];
const int dest_chk[16*4] = {0, 0, 0, 0, 9, 25, 11, 35, 0, 0, 0, 0, 17, 65, 19,
    75, 0, 0, 0, 0, 25, 105, 27, 115, 0, 0, 0, 0, 33, 145, 35, 155, 0, 0, 0,
    0, 41, 185, 43, 195, 0, 0, 0, 0, 49, 225, 51, 235, 0, 0, 0, 0, 57, 265, 59,
    275, 0, 0, 0, 0, 65, 305, 67, 315};

int main()
{
  check_vect ();
#pragma GCC novector
  for (int i = 0; i < 16; ++i)
    cond[i] = i & 1;
#pragma GCC novector
  for (int i = 0; i < 16 * 4; ++i)
    src[i] = i;
  test4 (dest, src, cond, 5, 16);
#pragma GCC novector
  for (int i = 0; i < 16 * 4; ++i)
    if (dest[i] != dest_chk[i])
      abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "STORE_LANES" "vect" { target { vect_variable_length && vect_load_lanes } } } } */
