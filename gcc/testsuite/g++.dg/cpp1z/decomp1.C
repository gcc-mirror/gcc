// { dg-do run { target c++11 } }
// { dg-options "" }

int a[2] = { 1, 2 };
struct S { int a; signed char b; float c; } s = { 6, 7, 8.0f };

int
main ()
{
  auto & [ c, d ] = a;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  auto [ e, f ] = a;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  auto [ g, h, i ] = s;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  auto & [ j, k, l ] = s;	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  c++;
  d++;
  e += 6;
  f += 7;
  g++;
  h++;
  j += 10;
  k += 11;
  if (c != 2 || &c != &a[0]
      || d != 3 || &d != &a[1]
      || e != 7 || &e == &a[0]
      || f != 9 || &f == &a[1]
      || g != 7 || &g == &s.a
      || h != 8 || &h == &s.b
      || i != 8.0f || &i == &s.c
      || j != 16 || &j != &s.a
      || k != 18 || &k != &s.b
      || l != 8.0f || &l != &s.c
      || a[0] != 2 || a[1] != 3
      || s.a != 16 || s.b != 18 || s.c != 8.0f)
    __builtin_abort ();
}
