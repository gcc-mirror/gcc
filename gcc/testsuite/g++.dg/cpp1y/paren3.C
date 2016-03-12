// PR c++/70106
// { dg-do compile { target c++14 } }

template <typename>
struct A
{
  int x;

  void foo () const {
    (A::x);
  }
};

struct B
{
  int x;

  template <typename>
  void foo () const {
    (B::x);
  }
};

void
foo ()
{
  A<int> ().foo ();
  B ().foo<int> ();
}

