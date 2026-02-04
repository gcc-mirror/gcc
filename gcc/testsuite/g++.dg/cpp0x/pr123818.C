// PR c++/123818
// { dg-do run { target c++11 } }

struct A { A (int x) : a (x) {} int a; };
struct B { A b; };

int
foo (B x = B { 42 })
{
    return x.b.a;
}

int
bar (B x = B { 43 })
{
    return x.b.a;
}

int
main ()
{
  if (foo () != bar () - 1)
    __builtin_abort ();
}
