//PR c++/27670

template<operator+> void foo(); // { dg-error "before|non-function|template" }

void bar()
{
  foo();                        // { dg-error "no matching function" }
  // { dg-message "(candidate|deduce template parameter)" "candidate note" { target *-*-* } 7 }
}
 
