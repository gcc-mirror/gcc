// PR c++/27572
// { dg-do compile }

template<typedef> void foo();  // { dg-error "10:typedef declaration" }
// { dg-error "no type|template" "" { target *-*-* } .-1 }

void bar()
{
  foo<int>();                  // { dg-error "matching" }
}
