// { dg-do run  }
// GROUPS passed bit-fields
// bitfield file
// Message-Id: <92Oct29.191913est.62@jarvis.csri.toronto.edu>
// From: mdivax1!robinson@ai.mit.edu (Jim Robinson)
// Subject: gcc 2.2.2 C++ bug in handling :0 bit fields
// Date:   Thu, 29 Oct 1992 19:18:28 -0500
//
// Also applies to:
// bitfield file
// From: Jaimie Wilson/MSL <Jaimie_Wilson@msl.isis.org>
// Date:   Fri, 28 Jan 1994 06:11:43 -0500
// Subject: GCC bug report
//
// This test is only meant for targets where EMPTY_FIELD_BOUNDARY is
// defined to no larger than the size of an unsigned int, or where
// PCC_BITFIELD_TYPE_MATTERS is defined.  Add skips below for targets that
// do not have that property.
// { dg-skip-if "" { mmix-knuth-mmixware } }


#include <stdio.h>
#include <stddef.h>

struct foo {
        char a;
        char b;
        unsigned int : 0;       /* force word alignment */
        char c;
};

int
main(int argc, char **argv)
{
        struct foo bar;

	if (offsetof (struct foo, c) > sizeof (unsigned int))
		{ printf ("FAIL\n"); return 1; }
	else
		printf ("PASS\n");
	return 0;
}

