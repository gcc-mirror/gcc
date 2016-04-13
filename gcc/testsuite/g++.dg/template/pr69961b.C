// PR c++/69961

struct A { A (int); };

template <typename T>
void foo ()
{
  A::A ((T)0); // { dg-error "cannot call constructor .A::A. directly" }
}

void
bar ()
{
  foo<int> ();
}
