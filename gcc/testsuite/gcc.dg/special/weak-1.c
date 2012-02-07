/* { dg-do run { xfail { hppa*-*-hpux* && { ! lp64 } } } } */
/* { dg-require-weak "" } */
/* { dg-additional-sources weak-1a.c } */
/* See PR target/23387 for hppa xfail details.  */

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
