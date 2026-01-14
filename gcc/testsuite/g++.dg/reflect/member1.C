// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test various forms of member access.

struct A {
  int val;
};

struct B {
  A a;
};

struct S {
  int x;
  static constexpr int sx = 42;
  void fn (int) { }
  template<typename T>
  void tfn (T) { }
  template<typename>
  struct N { };
  template<typename T>
  static T var;
  B b;
};

template<typename T>
struct C {
  int x;
  static constexpr int sx = 42;
  void fn (int) { }
  template<typename U>
  void tfn (U) { }
  template<typename>
  struct N { };
  template<typename U>
  static U var;
  B b;
};

void
f ()
{
  S s;
  S *sp = &s;
  C<int> c;
  C<int> *cp = &c;

  s.[: ^^S::x :] = 1;
  sp->[: ^^S::x :] = 2;
  s.[: ^^S::fn :](42);
  sp->[: ^^S::fn :](42);
  int a = s.[: ^^S::sx :];
  a += sp->[: ^^S::sx :];
  s.template [: ^^S::tfn :](42);
  sp->template [: ^^S::tfn :](42);
  s.template [: ^^S::tfn :]<int>(42);
  sp->template [: ^^S::tfn :]<int>(42);
  s.[: ^^S::var :]<int> = 1; // { dg-error "reflection .var<int>. not usable in a splice expression with template arguments" }
  s.template [: ^^S::var :]<int> = 1;
  sp->[: ^^S::var :]<int> = 1; // { dg-error "reflection .var<int>. not usable in a splice expression with template arguments" }
  sp->template [: ^^S::var :]<int> = 1;
  s.[: ^^S::b :].[: ^^B::a :].val;
  sp->[: ^^S::b :].[: ^^B::a :].val;
  [: ^^s :].[: ^^S::b :].[: ^^B::a :].val;
  [: ^^sp :]->[: ^^S::b :].[: ^^B::a :].val;

  c.[: ^^C<int>::x :] = 1;
  cp->[: ^^C<int>::x :] = 1;
  c.[: ^^C<int>::fn :](42);
  cp->[: ^^C<int>::fn :](42);
  a += c.[: ^^C<int>::sx :];
  a += cp->[: ^^C<int>::sx :];
  c.template [: ^^C<int>::tfn :](42);
  cp->template [: ^^C<int>::tfn :](42);
  c.template [: ^^C<int>::tfn :]<int>(42);
  cp->template [: ^^C<int>::tfn :]<int>(42);
  c.[: ^^C<int>::var :]<int> = 1; // { dg-error "reflection .var<int>. not usable in a splice expression with template arguments" }
  c.template [: ^^C<int>::var :]<int> = 1;
  cp->[: ^^C<int>::var :]<int> = 1; // { dg-error "reflection .var<int>. not usable in a splice expression with template arguments" }
  cp->template [: ^^C<int>::var :]<int> = 1;
  c.[: ^^C<int>::b :].[: ^^B::a :].val;
  cp->[: ^^C<int>::b :].[: ^^B::a :].val;
  [: ^^c :].[: ^^C<int>::b :].[: ^^B::a :].val;
  [: ^^cp :]->[: ^^C<int>::b :].[: ^^B::a :].val;

  [: ^^S :]::template N<int> n1;
  [: ^^C<int> :]::template N<int> n2;
  [: ^^C :]<int>::template N<int> n3;  // { dg-error "missing .template.|expected" }
  typename [: ^^C :]<int>::template N<int> n4;

  s->[: ^^S::x :] = 2; // { dg-error "non-pointer type" }
  sp.[: ^^S::x :] = 2; // { dg-error "which is of pointer type" }
  c.[: ^^C<char>::x :] = 1; // { dg-error "is not a base of" }
  cp->[: ^^C<char>::x :] = 1; // { dg-error "is not a base of" }
  s.template [: ^^S::N :].t;  // { dg-error "reflection .S::N. not usable in a template splice" }
  S::template [: ^^S::N<int> :] e1;  // { dg-error "expected unqualified-id" }
  C<int>::template [: ^^S::N<int> :] e2;  // { dg-error "expected unqualified-id" }
  s.template [: ^^S::var<int> :] = 1;  // { dg-error "reflection .S::var<int>. not usable in a template splice" }
}
