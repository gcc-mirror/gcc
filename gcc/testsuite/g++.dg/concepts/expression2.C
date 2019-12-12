// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template<typename T>
concept bool C1()
{
  return requires (T t) { t.f(); }; // { dg-message "in requirements" }
}

template<typename T>
concept bool C2()
{
  return requires { typename T::type; }; // { dg-message "in requirements" }
}

template<typename T>
  requires C1<T>()
void f1(T x) { }

template<typename T>
  requires C2<T>()
void f2(T x) { }

// Note that these declarations are private and therefore
// cannot satisfy the constraints.
class S
{
  using type = int;
  void f() { }
} s;

int main()
{
  f1(s); // { dg-error "unsatisfied|private" }
  f2(s); // { dg-error "" }

  // When used in non-SFINAE contexts, make sure that we fail
  // the constraint check before emitting the access check
  // failures. The context is being presented consistently
  // in both cases.
  static_assert(C1<S>(), ""); // { dg-error "failed" }
  static_assert(C2<S>(), ""); // { dg-error "" }
}
