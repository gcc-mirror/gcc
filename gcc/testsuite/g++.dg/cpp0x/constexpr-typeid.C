// { dg-options -std=c++0x }

#include <typeinfo>

struct A { virtual void f(); };

extern constexpr const std::type_info* p1 = &typeid(int);
extern constexpr const std::type_info* p2 = &typeid(A);
// typeid-expression whose operand is of a polymorphic class type
extern constexpr const std::type_info* p3 = &typeid((A())); // { dg-error "" "" { xfail *-*-* } }
