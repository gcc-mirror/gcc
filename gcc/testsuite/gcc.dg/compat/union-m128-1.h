#include <xmmintrin.h>

typedef struct
{
  __m128 x;
} SS_struct_mi128;

typedef union
{
  __m128 x;
} SS_union_mi128;

typedef union
{
  __m128 x;
  unsigned long long u[2];
} union_mi128;
