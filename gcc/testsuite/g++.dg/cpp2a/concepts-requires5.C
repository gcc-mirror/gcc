// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts -fconcepts-diagnostics-depth=2" }

// req9.C

template<class T, class U> concept same_as = __is_same(T, U);

template<typename T>
struct S1 { };

template<typename T>
concept C = requires(T x) { { x.fn() } -> same_as<S1<T>>; };

template<typename U>
  requires C<U>
void fn(U x)
{
  x.fn();
}

struct S2
{
  auto fn() const { return S1<S2>(); }
};

int driver_1()
{
  fn(S2{});
  return 0;
}

// req10.C
// Test implicit conversion requirements

template<typename T, typename U>
concept ConvertibleTo = requires(T& t) { {t} -> U&; }; // { dg-error "inaccessible|return-type-requirement" }

struct B { };
class D : /*private*/ B { };

void driver_2()
{
  static_assert(ConvertibleTo<D, B>()); // { dg-error "cannot call" }
  static_assert(ConvertibleTo<D, B>); // { dg-error "static assertion failed" }
}
