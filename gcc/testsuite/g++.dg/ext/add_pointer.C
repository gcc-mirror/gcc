// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

class ClassType { };

SA(__is_same(__add_pointer(int), int*));
SA(__is_same(__add_pointer(int*), int**));
SA(__is_same(__add_pointer(const int), const int*));
SA(__is_same(__add_pointer(int&), int*));
SA(__is_same(__add_pointer(ClassType*), ClassType**));
SA(__is_same(__add_pointer(ClassType), ClassType*));
SA(__is_same(__add_pointer(void), void*));
SA(__is_same(__add_pointer(const void), const void*));
SA(__is_same(__add_pointer(volatile void), volatile void*));
SA(__is_same(__add_pointer(const volatile void), const volatile void*));

void f1();
using f1_type = decltype(f1);
using pf1_type = decltype(&f1);
SA(__is_same(__add_pointer(f1_type), pf1_type));

void f2() noexcept; // PR libstdc++/78361
using f2_type = decltype(f2);
using pf2_type = decltype(&f2);
SA(__is_same(__add_pointer(f2_type), pf2_type));

using fn_type = void();
using pfn_type = void(*)();
SA(__is_same(__add_pointer(fn_type), pfn_type));

SA(__is_same(__add_pointer(void() &), void() &));
SA(__is_same(__add_pointer(void() & noexcept), void() & noexcept));
SA(__is_same(__add_pointer(void() const), void() const));
SA(__is_same(__add_pointer(void(...) &), void(...) &));
SA(__is_same(__add_pointer(void(...) & noexcept), void(...) & noexcept));
SA(__is_same(__add_pointer(void(...) const), void(...) const));

SA(__is_same(__add_pointer(void() __restrict), void() __restrict));
