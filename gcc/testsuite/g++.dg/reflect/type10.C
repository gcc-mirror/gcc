// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^void);

void fn1 ([: ^^int :]);  // { dg-error "declared void|expected a reflection of an expression" }
void fn2 ([: ^^int :][]);  // { dg-error "declared void|expected a reflection of an expression" }
void fn3 (typename [: ^^int :]);
void fn4 (typename [: ^^int :] *);

constexpr auto r = ^^fn3;
void fn3 ([: r :]); // { dg-error "declared void|expected a reflection of an expression" }

template<info R, info T>
void
g ()
{
  void foo(typename [:R:]);
  foo (nullptr);

  int bar(typename [:T:]);
  bar ({});
}

struct X { };

void
f ()
{
  g<^^int*, ^^X>();
}
