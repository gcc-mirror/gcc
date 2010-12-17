// PR c++/45588
// { dg-do compile }
// { dg-options "-Wunused" }

void bar (unsigned char *);

template <int N>
struct S
{
  static const int k = 6;
};

template <int N>
const int S<N>::k;

template <int N>
void
foo ()
{
  const int i = S<N>::k;
  unsigned char a[i];
  bar (a);
}

void
baz ()
{
  foo<0> ();
}
