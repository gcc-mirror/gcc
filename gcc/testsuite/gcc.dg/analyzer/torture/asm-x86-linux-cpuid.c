/* { dg-do assemble { target x86_64-*-* } } */
/* { dg-require-effective-target lp64 } */

#include "../analyzer-decls.h"

typedef unsigned __INT32_TYPE__ u32;
typedef unsigned __INT64_TYPE__ u64;

extern void check_init_u32 (u32 v);
extern void check_init_u64 (u32 v);

/* Adapted from linux kernel: arch/x86/include/asm/processor.h (GPL-2.0).  */

static inline void native_cpuid(unsigned int *eax, unsigned int *ebx,
				unsigned int *ecx, unsigned int *edx)
{
	/* ecx is often an input as well as an output. */
	asm volatile("cpuid"
	    : "=a" (*eax),
	      "=b" (*ebx),
	      "=c" (*ecx),
	      "=d" (*edx)
	    : "0" (*eax), "2" (*ecx)
	    : "memory");
}

static inline void cpuid(unsigned int op,
			 unsigned int *eax, unsigned int *ebx,
			 unsigned int *ecx, unsigned int *edx)
{
	*eax = op;
	*ecx = 0;
	native_cpuid(eax, ebx, ecx, edx);
}

void test_1 (void)
{
  u32 eax, ebx, ecx, edx;
  cpuid(0x8000001e, &eax, &ebx, &ecx, &edx); /* from "amd_get_topology".  */

  /* Verify that they are now initialized.  */
  check_init_u32 (eax);
  check_init_u32 (ebx);
  check_init_u32 (ecx);
  check_init_u32 (edx);
}
