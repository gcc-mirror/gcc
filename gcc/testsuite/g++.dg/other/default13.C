// PR c++/85028

struct A;

template < typename > struct B
{
  B (int, A = A()) : f (0) {}  // { dg-error "incomplete type" }
  int f;
};

B < int > b (0);
