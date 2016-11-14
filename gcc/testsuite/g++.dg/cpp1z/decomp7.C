// { dg-do run { target c++11 } }
// { dg-options "" }

int a[2] = { 1, 2 };
int b[2] = { 4, 5 };
struct S { int a; signed char b; float c; } sa = { 6, 7, 8.0f };
S sb = { 9, 10, 11.0f };

template <typename T, typename U>
void
foo (T &x, U &y)
{
  auto & [ c, d ] = a;		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  auto [ e, f ] = a;		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  auto [ g, h, i ] = sa;	// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  auto & [ j, k, l ] = sa;	// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  auto & [ m, n ] = x;		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  auto [ o, p ] = x;		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  auto [ q, r, s ] = y;		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  auto & [ t, u, v ] = y;	// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  c += 1;
  e += 2;
  g += 3;
  j += 4;
  m += 5;
  o += 6;
  q += 7;
  t += 8;
  if (c != 2 || &c != &a[0]
      || d != 2 || &d != &a[1]
      || e != 3 || &e == &a[0]
      || f != 2 || &f == &a[1]
      || g != 9 || &g == &sa.a
      || h != 7 || &h == &sa.b
      || i != 8.0f || &i == &sa.c
      || j != 10 || &j != &sa.a
      || k != 7 || &k != &sa.b
      || l != 8.0f || &l != &sa.c
      || m != 9 || &m != &b[0]
      || n != 5 || &n != &b[1]
      || o != 10 || &o == &b[0]
      || p != 5 || &p == &b[1]
      || q != 16 || &q == &sb.a
      || r != 10 || &r == &sb.b
      || s != 11.0f || &s == &sb.c
      || t != 17 || &t != &sb.a
      || u != 10 || &u != &sb.b
      || v != 11.0f || &v != &sb.c
      || a[0] != 2 || a[1] != 2
      || sa.a != 10 || sa.b != 7 || sa.c != 8.0f
      || b[0] != 9 || b[1] != 5
      || sb.a != 17 || sb.b != 10 || sb.c != 11.0f)
    __builtin_abort ();
}

int
main ()
{
  foo (b, sb);
}
