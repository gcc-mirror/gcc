/* { dg-do run { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

#include "test-unwind.h"

#if !defined __PIC__ && !defined __USING_SJLJ_EXCEPTIONS__
void __attribute__ ((noinline)) foo()
{
        ALTER_REGS();
        // Throw the except and expect returning to main
        throw 1;
}
#endif
