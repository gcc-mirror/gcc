// PR c++/44703
// { dg-do compile { target c++11 } }

#include <initializer_list>

typedef std::initializer_list<int> type ;
void f(type) {}

int main()
{
//  error: could not convert '{1, 2, 3}' to 'type'
    f({1,2,3}) ;
}

