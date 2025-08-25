// C++26 P1306R5 - Expansion statements
// { dg-do compile { target c++11 } }
// { dg-options "" }

namespace std {
  template <typename T>
  struct initializer_list {
  private:
    T *a;
    decltype (sizeof 0) b;
  public:
    constexpr decltype (sizeof 0) size () const noexcept { return b; }
    constexpr const T *begin () const noexcept { return a; }
    constexpr const T *end () const noexcept { return begin () + size (); }
};
}

struct A {};
struct B { int b; B () : b (42) {} };
struct C : public B { int c; C () : c (42), B () {} };
extern int f[];

void
foo (int n)
{
  int c[0] = {}, d[n];
  int e = 42;
  d[0] = 42;
  template for (auto a : A {})		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;
  template for (int b : B {})		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;
  template for (int i : c)		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;
  template for (int i : d)		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;					// { dg-error "cannot decompose variable length array" "" { target *-*-* } .-1 }
  template for (auto a : C {})		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;					// { dg-error "cannot decompose class type 'C': both it and its base class 'B' have non-static data members" "" { target *-*-* } .-1 }
  template for (auto a : e)		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;					// { dg-error "cannot decompose non-array non-class type 'int'" "" { target *-*-* } .-1 }
  template for (auto a : { .id1 = 5, .id2 = 6LL }) // { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;					// { dg-error "designators in 'template for' initializer" "" { target *-*-* } .-1 }
  template for (auto a : { .id3 { 5 }, .id4 = { 1.0 } }) // { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;					// { dg-error "designators in 'template for' initializer" "" { target *-*-* } .-1 }
  template for (int i : f)		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    ;					// { dg-error "cannot decompose array of unknown bound 'int \\\[\\\]'" "" { target *-*-* } .-1 }
}
