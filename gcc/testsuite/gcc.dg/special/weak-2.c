/* { dg-do run } */
/* { dg-require-weak "" } */
/* { dg-additional-sources "weak-2a.c weak-2b.c" } */

#include <stdlib.h>

extern int foo(void);

int main(void) {

    if (foo())
        exit(0);
    else
        abort();
}
