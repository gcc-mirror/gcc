// Make sure that the requirement fails because a .* expression of function
// type can only be used in a call.

// { dg-do compile { target concepts } }

template<class D, class T>
constexpr decltype(auto) invoke(D (T::*pmd), T&& t)
  noexcept(noexcept(t.*pmd))
  requires requires { t.*pmd; }
 { return t.*pmd; }

char invoke(...);

struct A
{
  int f();
};

int main()
{
  static_assert(sizeof(invoke (&A::f, A())) == 1);
}
