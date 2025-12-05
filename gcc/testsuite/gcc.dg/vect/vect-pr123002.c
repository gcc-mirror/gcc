/* { dg-additional-options "-mavx2" { target avx2 } } */

#include "tree-vect.h"

unsigned int enc_table_32[8][3] = {
    {513735U, 77223048U, 437087610U },
    {0U,      78508U,    646269101U },
    {0U,      0U,        11997U,    },
    {0U,      0U,        0U,        },
    {0U,      0U,        0U,        },
    {0U,      0U,        0U,        },
    {0U,      0U,        0U,        },
    {0U,      0U,        0U,        }};

int __attribute__((noipa)) foo()
{
  unsigned long intermediate[3] = {0};

  for (unsigned long i = 0UL; i < 8; i++) {
      intermediate[0] += 2 * (unsigned long)(enc_table_32)[i][0];
      intermediate[1] += 2 * (unsigned long)(enc_table_32)[i][1];
      intermediate[2] += 2 * (unsigned long)(enc_table_32)[i][2];
  }

  if (intermediate[0] == 0xfad8e &&
      intermediate[1] == 0x9370e68 && intermediate[2] == 0x8125ca08) {
      return 0;
  } else {
      return 1;
  }
}

int main()
{
  check_vect ();
  if (foo ())
    __builtin_abort ();
  return 0;
}
