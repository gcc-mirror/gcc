// { dg-do compile { target c++11 } }

// From N2235

// 4.5.3 constant expressions

// p 4
struct A {
  constexpr A(int i) : val(i) { }
  constexpr operator int() const { return val; }
  constexpr operator long() const { return -1; }
private:
  int val;
};

template<int I> struct X { static const int i = I; };
constexpr A a = 42;

X<a> x;	    // OK: unique conversion to int
int ar[X<a>::i]; // also OK
int ary[a]; // { dg-error "could not convert" } ambiguous conversion
// { dg-error "9:size of array .ary. has non-integral" "" { target c++11 } .-1 }
