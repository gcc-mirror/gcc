// { dg-do assemble  }
// GROUPS passed templates membertemplates
template <class X>
struct R
{
};


template <class T>
struct S
{
  template <class U>
  S(R<U> r);
};


void foo()
{
  R<int> r;
  S<char*> s(r);
}
