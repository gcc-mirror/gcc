/* { dg-do run } */
/* { dg-options "-Os -fno-tree-switch-conversion -ffunction-sections -fdata-sections -mrelax -Wl,--section-start=.foo=0x1fffa" } */

/* Make sure jumptables work properly if placed straddling 128 KB i.e
   some entries below 128 KB and some above it, with relaxation disabled. */

#include "exit-abort.h"
#include "pr71151-common.h"

int main()
{
	/* Not meant for devices with flash <= 128K */
#if defined (__AVR_2_BYTE_PC__)
	exit(0);
#else
	foo(5);
	if (y != 37)
		abort();

	foo(0);
	if (y != 67)
		abort();

	foo(7);
	if (y != 98)
		abort();
#endif
}
