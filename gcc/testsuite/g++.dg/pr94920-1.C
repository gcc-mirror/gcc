/* PR tree-optimization/94920 */
/* { dg-additional-options "-Wno-psabi" } */
/* { dg-do run } */

#include "pr94920.C"

int main() {

    if (foo(0) != 0
        || foo(-42) != 42
        || foo(42) != 42
        || baz(-10) != 10
        || baz(-10) != 10) {
            __builtin_abort();
        }
    
    return 0;
}
