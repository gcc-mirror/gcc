// { dg-do assemble  }

template <int I>
struct A {
};

template <int I, int J>
struct B {
  operator A<3> ();
  operator B<3, 7> ();
};


template <int I, int J>
void f(B<I, J>);

template <int I>
void f(A<I>)
{
}

int main()
{
  // Deduction fails with the first `f'.  Since `3' is explicitly
  // specified, we don't try any deduction with the second `f'.  So,
  // we call the second `f'.
  f<3>(B<2, 7>());
}
