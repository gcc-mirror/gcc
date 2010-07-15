#include <stdlib.h>
#include "cpuid.h"
#include "m128-check.h"

/* We need a single SSE2 instruction here so the handler can safely skip
   over it.  */
#define ILL_INSN __asm__ volatile ("unpcklpd %xmm0,%xmm2")
#define ILL_INSN_LEN 4
#include "sol2-check.h"

static void sse2_test (void);

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run SSE2 test only if host has SSE2 support.  */
  if ((edx & bit_SSE2) && sol2_check ())
    sse2_test ();

  return 0;
}
