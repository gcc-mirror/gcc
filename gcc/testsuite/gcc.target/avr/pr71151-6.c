/* { dg-do run } */
/* { dg-options "-Os -fno-tree-switch-conversion -ffunction-sections -fdata-sections -mrelax -Wl,--section-start=.foo=0x20000" } */

#ifdef __AVR_3_BYTE_PC__
/* Make sure jumptables work properly if placed above 128 KB, i.e. 3 byte
   flash address for loading jump table entry and a jump table entry
   that is a stub, with relaxation enabled, after removing the special
   section placement hook. */
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
