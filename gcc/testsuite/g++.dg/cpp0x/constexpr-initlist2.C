// { dg-options -std=c++11 }

#include <initializer_list>

constexpr auto list = { 1, 2, 3, 4 };

#define SA(X) static_assert(X, #X)
SA(list.size() == 4);
SA(list.begin()[2] == 3);
SA(list.end()[-1] == 4);
