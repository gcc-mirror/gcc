// PR c++/52905
// { dg-options -std=c++11 }

#include <initializer_list>

enum E { e1, e2 };
struct A
{
  A(std::initializer_list<E>);	// { dg-message "A::A" }
  A(int, E);			// { dg-message "A::A" }
};

A a{e1,2};			// { dg-error "" }
