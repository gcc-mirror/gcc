// { dg-do compile { target c++2a } }

// A poor mans Integral concept.
template<typename T>
concept Integral = __is_same_as(T, int);

template<int N>
concept Nonnegative = N >= 0;

template<typename... Args>
concept UnaryPack = (sizeof...(Args) == 1);

template<typename... Args>
  requires Integral<Args...> // { dg-error "non-pack parameter" }
void f1();

template<typename... Args>
  requires Integral<Args>... // { dg-error "parameter packs not expanded|expected unqualified-id" }
void f2();

template<typename... Args>
  requires (Integral<Args> && ...)
void f3() { }

template<Integral... Args>
void f4() { }

// FIXME: This syntax is likely to be made invalid.
template<Nonnegative... Args> // { dg-error "does not constrain a type" }
void f5() { }

template<UnaryPack Arg> // requires UnaryPack<Arg>
void f6() { }

template<UnaryPack... Args> // requires (... && UnaryPack<Args>)
void f7() { }

void driver()
{
  f1<int, int>(); // { dg-error "" }
  f3<int, int>();
  f3<int, void>(); // { dg-error "" }
  f4<int, int>();
  f4<int, void>(); // { dg-error "" }
  f7<int>();
  f7<int, int>();
}
