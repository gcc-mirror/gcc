// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflections on reflection-names.  Invalid code.

class A { int x; };
template<typename T>
class B { T x; };
struct C { int x; };
template<typename T>
struct D { T x; };

template<typename T>
void foo (T) { }

template<typename T>
void foo (T, T) { }

void bar (int) { }
void bar (int, int) { }

void
g ()
{
  constexpr auto r1 = ^^A::x;	  // { dg-error "private within this context" }
  constexpr auto r2 = ^^B<int>::x;  // { dg-error "private within this context" }

  constexpr auto r3 = ^^C::x;
  int i1 = [: r3 :];	  // { dg-error "cannot implicitly reference a class member .C::x. through a splice" }
  [: r3 :];		  // { dg-error "cannot implicitly reference a class member .C::x. through a splice" }
  [: r3 :] = 0;		  // { dg-error "cannot implicitly reference a class member .C::x. through a splice" }

  constexpr auto r4 = ^^foo;  // { dg-error "reflection of an overload set" }
  constexpr auto r5 = ^^bar;  // { dg-error "reflection of an overload set" }
  constexpr auto r6 = ^^D::x; // { dg-error "expected" }
  constexpr auto r7 = ^^D<int>::x;
  [: r7 :];   // { dg-error "cannot implicitly reference a class member .D<int>::x. through a splice" }
}

void
f ()
{
  g ();
}
