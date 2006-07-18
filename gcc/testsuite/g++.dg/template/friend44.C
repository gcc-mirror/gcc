//PR c++/28260

template<int> struct A
{
  friend int foo(); // { dg-error "new declaration" }
};

void foo() { A<0> a; } // { dg-error "ambiguates old declaration" } 
