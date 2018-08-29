// P0683R1
// { dg-do compile { target c++11 } }
// { dg-options "" }

extern "C" void abort ();

int
foo ()
{
  return 2;
}

int a = foo ();
const int b = 0;
struct S {
  int c : 5 = 2 * a;			// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
  int d : 6 { c + a };			// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
					// { dg-warning "narrowing conversion of" "" { target *-*-* } .-1 }
  int e : true ? 7 : a = 3;
  int f : (true ? 8 : b) = d + a;	// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
  int g : (true ? 9 : b) { f + a };	// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
					// { dg-warning "narrowing conversion of" "" { target *-*-* } .-1 }
  int h : 1 || new int { 0 };
  int i = g + a;
};
S c;
template <bool V, int W>
struct U {
  int j : W = 3 * a;			// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
  int k : W { j + a };			// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
					// { dg-warning "narrowing conversion of" "" { target *-*-* } .-1 }
  int l : V ? 7 : a = 3;
  int m : (V ? W : b) = k + a;		// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
  int n : (V ? W : b) { m + a };	// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
					// { dg-warning "narrowing conversion of" "" { target *-*-* } .-1 }
  int o : 1 || new int { 0 };
  int p = n + a;
};
U<true, 10> d;

int
main ()
{
  a = 1;
  if (c.c != 4 || c.d != 6 || c.e != 0 || c.f != 8 || c.g != 10 || c.h != 0 || c.i != 12)
    abort ();
  if (d.j != 6 || d.k != 8 || d.l != 0 || d.m != 10 || d.n != 12 || d.o != 0 || d.p != 14)
    abort ();
  S s;
  U<true, 10> u;
  if (s.c != 2 || s.d != 3 || s.f != 4 || s.g != 5 || s.i != 6)
    abort ();
  if (u.j != 3 || u.k != 4 || u.m != 5 || u.n != 6 || u.p != 7)
    abort ();
}
