/* PR tree-optimization/106243 */
/* { dg-do run } */
/* { dg-options "-O2 -Wno-psabi" } */

#include "pr106243.c"

int main () {

    if (foo(3) != 1
        || bar(-6) != 0
        || baz(17) != 1
        || qux(-128) != 0
        || foo(127) != 1) {
            __builtin_abort();
        }

    return 0;
}
