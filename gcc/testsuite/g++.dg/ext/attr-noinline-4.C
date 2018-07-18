// PR c++/84665

struct S {} a[1];

template <int N>
void
foo ()
{
  __attribute__ ((noinline (a[0]))) int c = 0; // { dg-error "wrong number of arguments" }
}
