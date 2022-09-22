/* PR tree-optimization/104992 */
/* { dg-do run } */
/* { dg-options "-O2 -Wno-psabi" } */

#include "../gcc.dg/pr104992.c"

int main () {

    /* Should be true.  */
    if (!foo(6, 3)
        || !bar(12, 2)
        || !baz(34, 17)
        || !qux(50, 10)
        || !fred(16, 8)
        || !baz(-9, 3)
        || !baz(9, -3)
        || !baz(-9, -3)
        ) {
            __builtin_abort();
         }
    
    /* Should be false.  */
    if (foo(5, 30)
        || bar(72, 27)
        || baz(42, 15)) {
            __builtin_abort();
        }
    
    return 0;
}
