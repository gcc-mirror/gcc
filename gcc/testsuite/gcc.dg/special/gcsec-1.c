/* AIX gld supports garbage collection. But AIX gcc does not support 
   -ffunction-sections or -fdata-sections.  */
/* { dg-do run { xfail rs6000-*-aix* powerpc*-*-aix* } } */
/* { dg-require-gc-sections "" } */

/* { dg-options "-ffunction-sections -fdata-sections -Wl,--gc-sections" } */
/* { dg-options "-ffunction-sections -fdata-sections -Wl,--gc-sections -static" { target static } } */

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
