// GROUPS passed arg-matching
// arg-matching file
// Message-Id: <9305041759.AA04913@malachite.bbn.com>
// From: Dan Franklin <dan@diamond.bbn.com>
// Subject: overloaded function resolved incorrectly
// Date: Tue, 4 May 93 13:59:18 EDT

#include <stdio.h>

// Given the following overloaded function definitions

void ovf(unsigned long, short,         short) { printf ("PASS\n"); }
void ovf(          int, short, unsigned long) { printf ("FAIL\n"); }

// and the call
//
//   ovf(unsigned long, unsigned int, unsigned int)
//
// it seems to me (and to cfront) that this should resolve to ovf #1 above,
// but g++ resolves it to ovf #2.  Resolving to ovf #1 requires two conversions
// (unsigned int => short) while resolving to ovf #2 takes two conversions
// (unsigned long => int, unsigned int => short) and a promotion
// (unsigned int => unsigned long).

int main(int, char**)
{
    unsigned long pixmap = 0;
    unsigned int x = 0;
    unsigned int y = 0;

    ovf(pixmap, x, y);
    return 0;
}
