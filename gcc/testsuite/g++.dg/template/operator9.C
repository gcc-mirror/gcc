//PR c++/27670

template<operator+> void foo(); // { dg-error "before|parameter|template" }

void bar()
{
  foo();                        // { dg-error "no matching function" }
}
 
