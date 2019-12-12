//PR c++/27670

template<operator+> void foo(); // { dg-error "10:declaration" }
// { dg-error "expected|template" "" { target *-*-* } .-1 }

void bar()
{
  foo();                        // { dg-error "no matching function" }
}
 
