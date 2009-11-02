// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/37093

struct A {};

template <int A::* p>
int
foo(A* q)
{
  return q->*p;
}

template <typename T>
int
bar(int T::* p)
{
  return foo<p>(0);// { dg-error "(not a valid template arg|no matching func|pointer-to-member)" }
}

int i = bar<A>(0);

