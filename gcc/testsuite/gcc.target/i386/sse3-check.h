#include <stdio.h>
#include <stdlib.h>

#include "cpuid.h"

/* We need a single SSE3 instruction here so the handler can safely skip
   over it.  */
#define ILL_INSN __asm__ volatile ("movddup %xmm1,%xmm2")
#define ILL_INSN_LEN 4
#include "sol2-check.h"

static void sse3_test (void);

static void
__attribute__ ((noinline))
do_test (void)
{
  sse3_test ();
}

int
main ()
{
  unsigned int eax, ebx, ecx, edx;
 
  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;
 
  /* Run SSE3 test only if host has SSE3 support.  */
  if ((ecx & bit_SSE3) && sol2_check ())
    do_test ();

  return 0;
}
