// { dg-options "-std=c++0x" }
template<void (*... fp)()> struct A
{
  A() { fp(); } // { dg-error "not expanded|fp" }
};

void foo();

A<foo> a;
