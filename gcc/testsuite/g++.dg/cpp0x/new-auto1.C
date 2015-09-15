// PR c++/51911
// { dg-do compile { target c++11 } }

#include <initializer_list>

auto foo1 = new auto { 3, 4, 5 };  // { dg-error "22:initialization of new-expression for type 'auto'" }
auto bar1 = new auto { 2 };

auto foo2 = new auto ( 3, 4, 5 );  // { dg-error "22:initialization of new-expression for type 'auto'" }
auto bar2 = new auto ( 2 );
