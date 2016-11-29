// { dg-do run { target c++11 } }
// { dg-options "" }

typedef int V __attribute__((vector_size (4 * sizeof (int))));
V a = (V) { 1, 2, 3, 4 };
__complex__ double b = 5.0 + 6.0i;
__complex__ int c = 7 + 8i;

int
main ()
{
  auto & [ d, e, f, g ] = a;	// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  auto [ h, i, j, k ] = a;	// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  auto [ l, m ] = b;		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  auto & [ n, o ] = b;		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  auto & [ p, q ] = c;		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  auto [ r, s ] = c;		// { dg-warning "decomposition declaration only available with" "" { target c++14_down } }
  d += 10;
  e += 11;
  f += 12;
  g += 13;
  h += 14;
  i += 15;
  j += 16;
  k += 17;
  l = l * 2.;
  m = m * 3.;
  n = n * 3.;
  o = o * 2.;
  p += 18;
  q += 19;
  r += 22;
  s += 23;
  if (d != 11 || &d != &a[0]
      || e != 13 || &e != &a[1]
      || f != 15 || &f != &a[2]
      || g != 17 || &g != &a[3]
      || h != 15 || &h == &a[0]
      || i != 17 || &i == &a[1]
      || j != 19 || &j == &a[2]
      || k != 21 || &k == &a[3]
      || l != 10.0 || &l == &__real__ b
      || m != 18.0 || &m == &__imag__ b
      || n != 15.0 || &n != &__real__ b
      || o != 12.0 || &o != &__imag__ b
      || p != 25 || &p != &__real__ c
      || q != 27 || &q != &__imag__ c
      || r != 29 || &r == &__real__ c
      || s != 31 || &s == &__imag__ c
      || a[0] != 11 || a[1] != 13 || a[2] != 15 || a[3] != 17
      || b != 15.0 + 12.0i
      || c != 25 + 27i)
    __builtin_abort ();
}
