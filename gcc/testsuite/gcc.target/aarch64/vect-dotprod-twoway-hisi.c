/* { dg-do run } */
/* { dg-require-effective-target vect_dotprod_hisi } */
/* { dg-options "-static -O3 -ftree-vectorize -fdump-tree-vect-details -save-temps" } */
/* Ensure runtime correctness in the autovectorized two-way dot product operations.  */

#include <stdint.h>
#include <stdlib.h>
#pragma GCC target "+sme2"

uint32_t
udot2 (int n, uint16_t* data)  __arm_streaming
{
  uint32_t sum = 0;
  for (int i=0; i<n; i+=1) {
    sum += data[i] * data[i];
  }
  return sum;
}

int32_t
sdot2 (int n, int16_t* data)  __arm_streaming
{
  int32_t sum = 0;
  for (int i=0; i<n; i+=1) {
    sum += data[i] * data[i];
  }
  return sum;
}

int
main ()
{

  uint16_t u_input_nil[] = { [0 ... 3] = 0 };
  uint16_t u_input_min[] = { [0 ... 3] = 1 };
  uint16_t u_input_max[] = { [0 ... 3] = 32767};

  uint32_t u_nil_dotprod = udot2 (4, u_input_nil);
  uint32_t u_min_dotprod = udot2 (4, u_input_min);
  uint32_t u_max_dotprod = udot2 (4, u_input_max);

  if (u_nil_dotprod != 0
      || u_min_dotprod != 4
      || u_max_dotprod != 4294705156)
    abort ();

  int16_t s_input_nil[] = { [0 ... 3] = 0 };
  int16_t s_input_min[] = { [0 ... 3] = -23170 };
  int16_t s_input_max[] = { [0 ... 3] =  23170 };

  int32_t s_nil_dotprod = sdot2 (4, s_input_nil);
  int32_t s_min_dotprod = sdot2 (4, s_input_min);
  int32_t s_max_dotprod = sdot2 (4, s_input_max);

  if (s_nil_dotprod != 0
      || s_min_dotprod != 2147395600
      || s_max_dotprod != 2147395600)
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected" 46 "vect" } } */
/* { dg-final { scan-assembler "\[ \t\]udot\tz\[0-9\]+.s, z\[0-9\]+.h, z\[0-9\]+.h" } } */
/* { dg-final { scan-assembler "\[ \t\]sdot\tz\[0-9\]+.s, z\[0-9\]+.h, z\[0-9\]+.h" } } */
