// { dg-options "-fshow-column" }
//PR c++/28260

template<int> struct A
{
  friend int foo(); // { dg-error "14:ambiguating new declaration" }
};

void foo() { A<0> a; } // { dg-message "6:old declaration" } 
