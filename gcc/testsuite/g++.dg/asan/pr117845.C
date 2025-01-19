// PR c++/117845 - Initially reported case.
// { dg-do "compile" }
// { dg-options "-fsanitize=address" }

#include <initializer_list>

void l() {
    auto const ints = {0,1,2,3,4,5};
    for (int i : ints | h) { // { dg-error "was not declared" }
        __builtin_printf("%d ", i);
    }
}
