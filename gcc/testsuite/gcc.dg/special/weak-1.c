/* { dg-do run } */
/* { dg-require-weak "" } */
/* { dg-additional-sources weak-1a.c } */

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
