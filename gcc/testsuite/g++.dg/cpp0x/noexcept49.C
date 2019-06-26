// PR c++/86476 - noexcept-specifier is a complete-class context
// { dg-do compile { target c++11 } }

#define SA(X) static_assert(X, #X)

struct S {
  static void f1() noexcept(b);
  static constexpr auto b = true;
};

S s;
SA(noexcept(s.f1()));
