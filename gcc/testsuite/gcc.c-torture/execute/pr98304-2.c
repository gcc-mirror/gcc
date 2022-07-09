/* PR tree-optimization/98304 */

#include "../../gcc.dg/pr98304-1.c"

/* Runtime tests.  */
int main() {

    /* Signed tests.  */
    if (foo(-42) != -42
        || foo(0) != 0
        || foo(63) != 63
        || foo(64) != 0
        || foo(65) != 1
        || foo(99) != 35) {
            __builtin_abort();
        }
    
    /* Unsigned tests.  */
    if (bar(42) != 42
        || bar(0) != 0
        || bar(63) != 63
        || bar(64) != 0
        || bar(65) != 1
        || bar(99) != 35) {
            __builtin_abort();
        }

    /* Should not simplify.  */
    if (corge(13) != 13
        || thud(13) != 13
        || qux(13) != 13
        || quux(13) != 13) {
            __builtin_abort();
        }

    return 0;
}
