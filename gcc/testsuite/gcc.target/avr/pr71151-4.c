/* { dg-do run } */
/* { dg-options "-Os -fno-tree-switch-conversion -ffunction-sections -fdata-sections -mrelax -Wl,--section-start=.foo=0x10000" } */

#ifdef __AVR_HAVE_ELPM__
/* Make sure jumptables work properly if placed above 64 KB and below 128 KB,
   i.e. 3 byte flash address for loading jump table entry and 2 byte jump
   table entry, with relaxation enabled, after removing the special section
   placement hook. */
#define SECTION_NAME ".foo"
#else
/* No special jump table placement so that avrtest won't abort
   for, e.g. ATmega64.  */
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
