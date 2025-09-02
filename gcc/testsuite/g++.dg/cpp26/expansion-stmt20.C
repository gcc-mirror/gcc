// PR c++/121575
// { dg-do run { target c++11 } }
// { dg-options "" }

struct A { int x, y; };
int c;

void
qux (A p)
{
  if (p.x != 1 || p.y != 3)
    __builtin_abort ();
  ++c;
}

void
foo ()
{
  A p { 1, 3 };
  template for (auto _ : {})			// { dg-warning "'template for' only available with" "" { target c++23_down } }
    qux (p);
  template for (auto _ : { 0 })			// { dg-warning "'template for' only available with" "" { target c++23_down } }
    qux (p);
}

void
bar (A p)
{
  template for (auto _ : {})			// { dg-warning "'template for' only available with" "" { target c++23_down } }
    qux (p);
  template for (auto _ : { 0, 1 })		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    qux (p);
}

A
baz ()
{
  A p { 1, 3 };
  template for (auto _ : {})			// { dg-warning "'template for' only available with" "" { target c++23_down } }
    qux (p);
  template for (auto _ : { 0, 1, 2 })		// { dg-warning "'template for' only available with" "" { target c++23_down } }
    qux (p);
  return p;
}

int
main ()
{
  foo ();
  if (c != 1)
    __builtin_abort ();
  bar ({ 1, 3 });
  if (c != 3)
    __builtin_abort ();
  if (baz ().x != 1 || baz ().y != 3)
    __builtin_abort ();
  if (c != 9)
    __builtin_abort ();
}
