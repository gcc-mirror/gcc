/* { dg-do run } */
/* { dg-require-weak "" } */
/* { dg-additional-sources "weak-2a.c weak-2b.c" } */

/* NVPTX's implementation of weak is broken when a strong symbol is in
   a later object file than the weak definition.   */
/* { dg-skip-if "" { "nvptx-*-*" } "*" { "" } } */

#include <stdlib.h>

extern int foo(void);

int main(void) {

    if (foo())
        exit(0);
    else
        abort();
}
