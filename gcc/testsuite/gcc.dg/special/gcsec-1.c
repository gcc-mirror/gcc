/* AIX gld supports garbage collection. But AIX gcc does not support 
   -ffunction-sections or -fdata-sections.  */
/* { dg-do run { xfail rs6000-*-aix* powerpc*-*-aix* } } */
/* { dg-require-gc-sections "" } */

/* { dg-options "-ffunction-sections -fdata-sections -Wl,--gc-sections -static" } */
/* { dg-options "-ffunction-sections -fdata-sections -Wl,--gc-sections -static" { target native } } */
/* Solaris 10 does not support static linking; there is no libc.a.  */
/* { dg-options "-ffunction-sections -fdata-sections -Wl,--gc-sections" { target *-*-netware* i?86-*-solaris2.1[0-9] } } */

#include <stdlib.h>

static int unusedint=5;

static int usedint=1;

int unused(void) {
    return 1;
}

int foo(void) {
    return usedint;
}

int main(void) {

    if (foo())
        exit(0);
    else
        abort();
}
