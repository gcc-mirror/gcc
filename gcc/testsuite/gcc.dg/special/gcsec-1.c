/* { dg-do run } */

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
