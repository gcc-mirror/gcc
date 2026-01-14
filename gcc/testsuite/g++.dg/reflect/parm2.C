// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// From [dcl.fct.default].

using info = decltype(^^int);

int b;
class X {
  int a;
  int mem1(int i = a);    // { dg-error "invalid use of non-static data member .X::a." }
  int mem2(int i = b);    // OK; use X::b
  consteval void mem3(info r = ^^a) {};    // OK
  int mem4(int i = [:^^a:]); // { dg-error "cannot implicitly reference" }

  static int b;
};
