// PR c++/98249

#include <new>
struct Incomplete;
template<class T> struct Holder { T t; };
Holder<Incomplete> *p;
void test() {
    ::new (p) int;
    new (p) int;
}
