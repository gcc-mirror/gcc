// PR c++/60305
// { dg-do compile { target c++11 } }

template<int I> int foo() { return I; }

template<int... I> void bar()
{
  constexpr int (*X[])() = { foo<I>... };
}

template void bar<1,3,5>();
