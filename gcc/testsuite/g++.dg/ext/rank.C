// { dg-do compile { target c++11 } }

#include <cstddef>

#define SA(X) static_assert((X),#X)

class ClassType { };

SA(__array_rank(int) == 0);
SA(__array_rank(int[2]) == 1);
SA(__array_rank(int[][4]) == 2);
SA(__array_rank(int[2][2][4][4][6][6]) == 6);
SA(__array_rank(ClassType) == 0);
SA(__array_rank(ClassType[2]) == 1);
SA(__array_rank(ClassType[][4]) == 2);
SA(__array_rank(ClassType[2][2][4][4][6][6]) == 6);

template<class T> void f(T) = delete;
void f(size_t);

template<class T>
void g() { f(__array_rank(T)); }

template void g<int>();
