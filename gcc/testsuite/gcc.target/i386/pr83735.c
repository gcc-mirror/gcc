/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O3 -mavx" } */

#include <stdlib.h>
#include "cpuid.h"
#include "m256-check.h"
#include "avx-os-support.h"

static void __attribute__((constructor))
check_avx (void)
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    exit (0);

  /* Run AVX test only if host has AVX support.  */
  if (((ecx & (bit_AVX | bit_OSXSAVE)) == (bit_AVX | bit_OSXSAVE))
      && avx_os_support ())
    return;

  exit (0);
}

struct S
{
  short b;
  long c;
  char d;
  long e;
  unsigned:8;
};

int f, h, k, l;
int g[10];
volatile struct S j;
char m;

int
main (void)
{
  int i;
  struct S n;
  for (i = 0; i < 6; i++)
    {
      for (f = 0; f < 10; f++)
	g[f] = 4;
      n = j;
      h = m == 0 ? 1 : 5 % m;
      if (l)
	n.b = k;
    }
  return n.b;
}
