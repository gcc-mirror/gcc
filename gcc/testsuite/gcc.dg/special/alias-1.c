/* { dg-do link } */
/* { dg-require-alias "" } */

#include <stdlib.h>

extern int foo(void) __attribute__((alias("bar")));

int bar(void) {
    return 1;
}

int main(void) {

    if (foo())
        exit(0);
    else
        abort();
}
