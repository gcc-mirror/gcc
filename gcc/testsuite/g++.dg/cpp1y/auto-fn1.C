// { dg-do compile { target c++1y } }

constexpr auto f() { return (char)42; }
#define SA(X) static_assert ((X),#X)
SA (f() == 42);
