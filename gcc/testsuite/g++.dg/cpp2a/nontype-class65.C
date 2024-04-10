// PR c++/113649
// { dg-do compile { target c++20 } }

template<class... Args>
struct A {
  template<class Ret>
  struct Fun { constexpr Fun(Ret(*)(Args...)) { } };

  template<Fun f>
  struct B { using type = decltype(f); };
};

bool f(char, long);

using type = A<char, long>::B<&f>::type;
using type = A<char, long>::Fun<bool>;
