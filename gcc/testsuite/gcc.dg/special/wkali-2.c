/* { dg-do run } */

#include <stdlib.h>

extern int foo(void);

int main(void) {

    if (foo())
        exit(0);
    else
        abort();
}
