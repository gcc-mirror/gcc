/* { dg-do run } */
/* { dg-require-weak "" } */
/* { dg-require-alias "" } */
/* { dg-additional-sources "wkali-2a.c wkali-2b.c" } */

#include <stdlib.h>

extern int foo(void);

int main(void) {

    if (foo())
        exit(0);
    else
        abort();
}
