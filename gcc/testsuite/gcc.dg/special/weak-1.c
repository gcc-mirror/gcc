/* { dg-do run } */

#include <stdlib.h>

int foo(void) __attribute__((weak));

int foo(void) {
    return 0;
}

int main(void) {

    if (foo())
        exit(0);
    else
        abort();
}
