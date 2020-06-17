// { dg-do compile { target c++20 } }

template<typename T>
concept Class = __is_class(T);

// Allow a requires-expression with no parms.
template<typename T>
concept C = requires { typename T::type; };

void f1(int a) requires true;         // { dg-error "non-templated" }
auto f2(int a) -> bool requires true; // { dg-error "non-templated" }
auto f3(int a) requires true -> bool; // { dg-error "" } requires-clause precedes trailing-return-type
typedef void fn_t() requires true;    // { dg-error "typedef" }
void (*pf)() requires true;           // { dg-error "non-function" }
void (*fn(int))() requires false;     // { dg-error "return type" }
void g(int (*)() requires true);      // { dg-error "parameter|non-function" }
auto* p = new (void(*)(char) requires true); // { dg-error "type-id" }
void f4(auto a) requires Class<decltype(a)> { }
void f5(auto a) requires requires (decltype(a) x) { -x; } { } // { dg-message "in requirements" }

struct Test {
  void f(auto a) requires Class<decltype(a)>;
} test;

void driver_1() {
  struct S { } s;
  f4(s);
  f5(0);
  f5((void*)0); // { dg-error "" }
  test.f(s);
}

void Test::f(auto a) requires Class<decltype(a)> { }

template<bool B> requires B struct S0; // OK

template<int N> requires N struct S1 { }; // { dg-error "does not have type" }
S1<1> x0; // { dg-error "template constraint failure|does not have type" }

template<int N> requires N == 0 struct S2 { }; // { dg-error "does not have type|must be enclosed" }

template<int N> requires (N == 0) struct S3 { }; // OK

template<typename T, T X> requires X struct S4 { }; // { dg-error "bool" }
S4<int, 0> x1;      // { dg-error "template constraint failure" }
S4<bool, true> x2; // OK
S4<bool, false> x3; // { dg-error "template constraint failure" }


// req11.C
template<typename T>
concept Streamable = requires (T t) { t; };

template<typename T>
concept Range = requires (T t) { t; };

// FIXME: There are two syntax errors here when there should be
// just one.Note that !Range<T> is not a primary-expression and needs to
// be wrapped in parens to be syntactically valid.
template<class T>
  requires Streamable<T> && !Range<T> // { dg-error "must be enclosed" }
void print1(const T& x) { }

template<class T>
  requires Streamable<T> && (!Range<T>)
void print2(const T& x) { }

void driver_3()
{
  print2("hello"); // { dg-error "" }
}
