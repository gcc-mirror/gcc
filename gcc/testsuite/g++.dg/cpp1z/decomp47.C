// PR c++/87122
// { dg-do run { target c++14 } }
// { dg-options "" }

extern "C" void abort ();
struct S { int a, b; };
int c;

template <int N>
void
foo ()
{
  S x[4] = { { N, 2 }, { 3, 4 }, { 5, 6 }, { 7, 8 } };
  auto f = [](auto & y) {
    for (auto & [ u, v ] : y)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
      {
	if ((u & 1) != 1 || v != u + 1 || u < N || u > 7 || (c & (1 << u))
	    || &u != &y[v / 2 - 1].a || &v != &y[v / 2 - 1].b)
	  abort ();
	c |= 1 << u;
      }
  };
  f (x);
}

int
main ()
{
  foo<1> ();
  if (c != 0xaa)
    abort ();
}
