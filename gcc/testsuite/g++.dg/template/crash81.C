// PR c++/34485

struct A
{
  template<T::X> struct X; // { dg-error "'T' has not been declared" "T" }
  // { dg-error "declaration of 'template<int X> struct A::X'" "A::X" { target *-*-* } 5 }
  // { dg-error "shadows template parm 'int X'" "shadow" { target *-*-* } 5 }
};
