/* { dg-do run } */
/* { dg-options "-Os -fno-tree-switch-conversion -ffunction-sections -fdata-sections" } */

/* Make sure jumptables work properly if placed below 64 KB i.e. 2 byte
   flash address for loading jump table entry, 2 byte entry, after
   removing the special section placement hook. */

#define SECTION_NAME ".foo"

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
