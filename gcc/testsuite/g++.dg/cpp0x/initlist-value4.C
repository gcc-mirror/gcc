// PR c++/88857
// { dg-do compile { target c++11 } }

class S { int a; };
void foo (const S &, int);

template <int N>
void
bar ()
{
  foo ({}); // { dg-error "too few arguments to function" }
}
