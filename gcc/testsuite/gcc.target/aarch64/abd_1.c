/* { dg-do compile } */
/* { dg-options "-O3" } */

#pragma GCC target "+nosve"

#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define N 1024

#define FUNC(T)								\
void									\
sabd_##T (signed T * restrict a, signed T * restrict b,		\
		signed T * restrict out)				\
{									\
  for (int i = 0; i < N; i++)						\
    out[i] = MAX (a[i], b[i]) - MIN (a[i], b[i]);			\
}									\
									\
void									\
uabd_##T (unsigned T * restrict a, unsigned T * restrict b,	\
		  unsigned T * restrict out)				\
{									\
  for (int i = 0; i < N; i++)						\
    out[i] = MAX (a[i], b[i]) - MIN (a[i], b[i]);			\
}

FUNC(char)
FUNC(short)
FUNC(int)

/* { dg-final { scan-assembler-times "sabd\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b, v\[0-9\]+\.16b" 1 } } */
/* { dg-final { scan-assembler-times "uabd\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b, v\[0-9\]+\.16b" 1 } } */
/* { dg-final { scan-assembler-times "sabd\\tv\[0-9\]+\.8h, v\[0-9\]+\.8h, v\[0-9\]+\.8h" 1 } } */
/* { dg-final { scan-assembler-times "uabd\\tv\[0-9\]+\.8h, v\[0-9\]+\.8h, v\[0-9\]+\.8h" 1 } } */
/* { dg-final { scan-assembler-times "sabd\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */
/* { dg-final { scan-assembler-times "uabd\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */
