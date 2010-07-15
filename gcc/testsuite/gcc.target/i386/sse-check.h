#include <stdlib.h>
#include "m128-check.h"

#include "cpuid.h"

/* We need a single SSE instruction here so the handler can safely skip
   over it.  */
#define ILL_INSN __asm__ volatile ("movss %xmm2,%xmm1")
#define ILL_INSN_LEN 4
#include "sol2-check.h"

static void sse_test (void);

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run SSE test only if host has SSE support.  */
  if ((edx & bit_SSE) && sol2_check ())
    sse_test ();

  return 0;
}
