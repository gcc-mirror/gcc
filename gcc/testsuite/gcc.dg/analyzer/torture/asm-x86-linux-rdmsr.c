/* { dg-do compile { target x86_64-*-* } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

#include "../analyzer-decls.h"

/* Adapted from Linux: arch/x86/include/asm/msr.h (GPL-2.0)  */

#ifdef __x86_64__
#define DECLARE_ARGS(val, low, high)	unsigned long long low, high
#define EAX_EDX_VAL(val, low, high)	((low) | (high) << 32)
#define EAX_EDX_RET(val, low, high)	"=a" (low), "=d" (high)
#else
#define DECLARE_ARGS(val, low, high)	unsigned long long val
#define EAX_EDX_VAL(val, low, high)	(val)
#define EAX_EDX_RET(val, low, high)	"=A" (val)
#endif

static unsigned long long __rdmsr(unsigned int msr)
{
	DECLARE_ARGS(val, low, high);

	asm volatile("1: rdmsr\n"
		     "2:\n"
		     : EAX_EDX_RET(val, low, high) : "c" (msr));

	return EAX_EDX_VAL(val, low, high);
}

void test (void)
{
  __analyzer_eval (__rdmsr (0)); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (__rdmsr (1)); /* { dg-warning "UNKNOWN" } */
}
