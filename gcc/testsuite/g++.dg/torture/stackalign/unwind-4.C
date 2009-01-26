/* { dg-do run { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

#include "test-unwind.h"

#if !defined __PIC__ && !defined __USING_SJLJ_EXCEPTIONS__
volatile int __attribute__ ((aligned(32))) g_a=1;
/* Test situation 4: no Drap and stack realign doesn't really happen */
void __attribute__ ((noinline))
foo()
{
	int i;
	ALTER_REGS();
	for (i=0; i < 10; i++)
		g_a++;
	throw g_a;
}
#endif
