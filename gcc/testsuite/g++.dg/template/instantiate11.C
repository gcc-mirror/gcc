// PR c++/42608
// { dg-do compile }

template <class U, class V>
struct A;

template <class V>
struct A<int, V>
{
  void f ();
};

template struct A<int, int>;

int
main ()
{
  A<int, int> a;
  a.f ();
  return 0;
}

// Make sure we get undefined reference error if
// A<int, int>::f () isn't instantiated elsewhere.
// { dg-final { scan-assembler-not "weak\[\n\t\]*_ZN1AIiiE1fEv" } }
