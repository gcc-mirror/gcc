// PR c++/27572
// { dg-do compile }

template<typedef> void foo();  // { dg-error "no type|typedef declaration" }

void bar()
{
  foo<int>();                  // { dg-error "matching" }
}
