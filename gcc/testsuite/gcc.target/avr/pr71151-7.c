/* { dg-do run } */
/* { dg-options "-Os -fno-tree-switch-conversion -ffunction-sections -fdata-sections -mno-relax -Wl,--section-start=.foo=0x1fffa" } */

#ifdef __AVR_3_BYTE_PC__
/* Make sure jumptables work properly if placed straddling 128 KB i.e
   some entries below 128 KB and some above it, with relaxation disabled. */
#define SECTION_NAME ".foo"
#else
/* No special jump table placement so that avrtest won't abort
   for, e.g. ATmega128.  */
#define SECTION_NAME ".text.foo"
#endif

#include "exit-abort.h"
#include "pr71151-common.h"

int main()
{
	foo(5);
	if (y != 37)
		abort();

	foo(0);
	if (y != 67)
		abort();

	foo(7);
	if (y != 98)
		abort();
}
