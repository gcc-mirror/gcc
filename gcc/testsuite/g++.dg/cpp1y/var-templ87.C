// PR c++/113976
// { dg-do run { target c++14 } }

int
foo ()
{
  return 42;
}

template <int N>
const int a = foo ();
const int *b = &a <0>;
template <int N>
const int c = foo ();
template const int c <0>;
template <int N>
const int d = foo ();
const int *e = &d <0>;
template const int d <0>;
template <int N>
const int f = foo ();
template const int f <0>;
const int *g = &f <0>;
struct S { int a, b; };
template <int N>
const S h = { 42, foo () };
const S *i = &h <0>;
template <int N>
const S j =  { 42, foo () };
template const S j <0>;
template <int N>
const S k =  { 42, foo () };
const S *l = &k <0>;
template const S k <0>;
template <int N>
const S m =  { 42, foo () };
template const S m <0>;
const S *n = &m <0>;

int
main ()
{
}
