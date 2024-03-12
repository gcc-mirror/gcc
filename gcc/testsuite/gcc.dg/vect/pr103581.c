/* { dg-additional-options "-mavx2 -mtune-ctrl=use_gather" { target avx2_runtime } } */

#include "tree-vect.h"

#define MASKGATHER(SUFF, TYPE1, TYPE2) \
TYPE1 * __attribute__((noipa)) \
maskgather ## SUFF (int n, TYPE2 *indices, TYPE1 *data) \
{ \
  TYPE1 *out = __builtin_malloc (sizeof (TYPE1) * n); \
  for (int i = 0; i < n; ++i) \
    { \
      TYPE2 d = indices[i]; \
      if (d > 1) \
        out[i] = data[d]; \
    } \
  return out; \
}

MASKGATHER(udiusi, unsigned long long, unsigned int)
MASKGATHER(usiusi, unsigned int, unsigned int)
MASKGATHER(udiudi, unsigned long long, unsigned long long)
MASKGATHER(usiudi, unsigned int, unsigned long long)

int
main()
{
  check_vect ();

    unsigned int idx4[32], data4[32];
  unsigned long long idx8[32], data8[32];
  for (int i = 0; i < 32; ++i)
    {
      idx4[i] = i;
      idx8[i] = i;
      data4[i] = i;
      data8[i] = i;
    }
  unsigned long long *resudiusi = maskgatherudiusi (16, idx4, data8);
  unsigned int *resusiusi = maskgatherusiusi (16, idx4, data4);
  unsigned long long *resudiudi = maskgatherudiudi (16, idx8, data8);
  unsigned int *resusiudi = maskgatherusiudi (16, idx8, data4);
#pragma GCC novector
  for (int i = 0; i < 16; ++i)
    {
      unsigned int d = idx4[i];
      if (d > 1)
        {
          if (resudiusi[i] != data4[d])
            __builtin_abort ();
          if (resudiudi[i] != data4[d])
            __builtin_abort ();
          if (resusiudi[i] != data4[d])
            __builtin_abort ();
          if (resusiusi[i] != data4[d])
            __builtin_abort ();
        }
    }
  return 0;
}

