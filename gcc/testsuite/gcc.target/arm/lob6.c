/* Check that GCC generates Armv8.1-M low over head loop instructions
   with some less trivial loops and the result is correct.  */
/* { dg-do run } */
/* { dg-require-effective-target arm_v8_1_lob_ok } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" "-mcpu=*" } } */
/* { dg-options "-march=armv8.1-m.main -mthumb -O3 --save-temps" } */
#include <stdlib.h>
#include "lob.h"

#define TEST_CODE1				\
  {						\
    for (int i = 0; i < N; i++)			\
      {						\
	a[i] = i;				\
	b[i] = i * 2;				\
						\
	for (int k = 0; k < N; k++)		\
	  {					\
	    MAYBE_LOB;				\
	    c[k] = k / 2;			\
	  }					\
	c[i] = a[i] - b[i];			\
      }						\
  }

#define TEST_CODE2				\
  {						\
    for (int i = 0; i < N / 2; i++)		\
      {						\
	MAYBE_LOB;				\
	if (c[i] % 2 == 0)			\
	  break;				\
	a[i]++;					\
	b[i]++;					\
      }						\
  }

int a1[N];
int b1[N];
int c1[N];

int a2[N];
int b2[N];
int c2[N];

#define MAYBE_LOB
void __attribute__((noinline))
loop1 (int *a, int *b, int *c)
  TEST_CODE1;

void __attribute__((noinline))
loop2 (int *a, int *b, int *c)
  TEST_CODE2;

#undef MAYBE_LOB
#define MAYBE_LOB NO_LOB

void
ref1 (int *a, int *b, int *c)
  TEST_CODE1;

void
ref2 (int *a, int *b, int *c)
  TEST_CODE2;

void
check (void)
{
  for (int i = 0; i < N; i++)
    {
      NO_LOB;
      if (a1[i] != a2[i]
	  && b1[i] != b2[i]
	  && c1[i] != c2[i])
	abort ();
    }
}

int
main (void)
{
  reset_data (a1, b1, c1);
  reset_data (a2, b2, c2);
  loop1 (a1, b1, c1);
  ref1 (a2, b2, c2);
  check ();

  reset_data (a1, b1, c1);
  reset_data (a2, b2, c2);
  loop2 (a1, b1, c1);
  ref2 (a2, b2, c2);
  check ();

  return 0;
}
/* { dg-final { scan-assembler-times {dls\s\S*,\s\S*} 1 } } */
/* { dg-final { scan-assembler-times {le\slr,\s\S*} 1 } } */
