// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
concept bool C1()
{
  return requires (T t) { t.f(); };
}

template<typename T>
concept bool C2()
{
  return requires { typename T::type; };
}

template<typename T>
  requires C1<T>()
void f1(T x) { }

template<typename T>
  requires C2<T>()
void f2(T x) { }

// Note that these declarations are private and therefore
// cannot satisify the constraints.
class S
{
  using type = int;
  void f() { }
} s;

int main()
{
  f1(s); // { dg-error "cannot call" }
  f2(s); // { dg-error "" }

  // When used in non-SFINAE contexts, make sure that we fail
  // the constraint check before emitting the access check
  // failures. The context is being presented constistently
  // in both cases.
  static_assert(C1<S>(), ""); // { dg-error "failed" }
  static_assert(C2<S>(), ""); // { dg-error "" }
}
