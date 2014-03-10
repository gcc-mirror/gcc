// { dg-do compile { target c++11 } }
template<void (*... fp)()> struct A
{
  A() { fp(); } // { dg-error "not expanded|fp" }
};

void foo();

A<foo> a;
