// { dg-do run  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T> struct A {
  template <class U> int f(U) { return 1; }
};

template <>
template <class U>
int A<int>::f(U) { return 0; }

A<int> a;

int main ()
{
  return a.f (3);
}
