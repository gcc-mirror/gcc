// PR c++/102651

#include <typeinfo>

template <typename T>
struct S : T{
    T x;
};

const void *p;
int main()
{
  p = &typeid( S<void>** );
}
