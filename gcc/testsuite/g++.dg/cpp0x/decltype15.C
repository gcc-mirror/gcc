// PR c++/38640
// { dg-do compile }
// { dg-options "-std=c++0x" }

template<int N> void foo (decltype (N));
template<long int N> void foo (decltype (N));

void
bar (void)
{
  foo<5> (6);
  foo<5L> (6L);
}
