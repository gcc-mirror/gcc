// PR c++/117845 - Actually valid variant
// { dg-do "compile" }
// { dg-options "-fsanitize=address" }

#include <initializer_list>

void l() {
    auto const ints = {0,1,2,3,4,5};
    for (auto i : { 3 } ) {
        __builtin_printf("%d ", i);
    }
}
