// PR c++/40370
// { dg-do compile }

struct A
{
  static int i;
};

template <int> struct B
{
  int x[A::i];	// { dg-error "array bound is not an integer constant" }
};
