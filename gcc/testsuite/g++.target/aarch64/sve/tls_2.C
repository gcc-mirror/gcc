/* { dg-do compile } */
/* { dg-require-effective-target tls } */
/* { dg-options "-O2 -fPIC -msve-vector-bits=256" } */
/* { dg-require-effective-target fpic } */

#include <stdint.h>

typedef int8_t v32qi __attribute__((vector_size (32)));

extern __thread int z;

void
foo (v32qi *a, int *b)
{
  v32qi x = a[0], y = a[1];
  asm volatile ("" :: "w" ((v32qi) { -1, 0, 0, -1, -1, -1, 0, 0,
				     -1, -1, -1, -1, 0, 0, 0, 0,
				     -1, -1, -1, -1, -1, -1, -1, -1,
				     0, 0, 0, 0, 0, 0, 0, 0 } ? x : y)
		: "memory");
  if (*b)
    {
      x = a[2], y = a[3];
      asm volatile ("" :: "w" ((v32qi) { -1, 0, 0, -1, -1, -1, 0, 0,
					 -1, -1, -1, -1, 0, 0, 0, 0,
					 -1, -1, -1, -1, -1, -1, -1, -1,
					 0, 0, 0, 0, 0, 0, 0, 0 } ? x : y),
		    "r" (z));
    }
}

/* { dg-final { scan-assembler-times {\tldr\tp[0-9]} 2 } } */
