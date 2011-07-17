// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/37093

struct A {};

template <int A::* p>
int
foo(A* q)			// { dg-message "note" }
{
  return q->*p;
}

template <typename T>
int
bar(int T::* p)
{
  return foo<p>(0);// { dg-error "(not a valid template arg|no matching func|pointer-to-member|could not convert)" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 17 }
}

int i = bar<A>(0);

