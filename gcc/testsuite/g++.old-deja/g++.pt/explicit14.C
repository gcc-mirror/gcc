// { dg-do assemble  }
// GROUPS passed templates
template <class T>
struct S
{
  template <class U>
  typename U::R foo(U u);
};


void bar()
{
  S<int> si;
}
