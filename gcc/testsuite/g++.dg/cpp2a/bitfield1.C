// P0683R1
// { dg-do run { target c++11 } }
// { dg-options "" }

extern "C" void abort ();
int a;
const int b = 0;
struct S {
  int c : 5 = 1;		// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
  int d : 6 { 2 };		// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
  int e : true ? 7 : a = 3;
  int f : (true ? 8 : b) = 4;	// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
  int g : (true ? 9 : b) { 5 };	// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
  int h : 1 || new int { 0 };
};
#if __cplusplus >= 201402L
static_assert (S{}.c == 1);
static_assert (S{}.d == 2);
static_assert (S{}.e == 0);
static_assert (S{}.f == 4);
static_assert (S{}.g == 5);
static_assert (S{}.h == 0);
#endif
template <bool V, int W>
struct U {
  int j : W = 7;		// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
  int k : W { 8 };		// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
  int l : V ? 7 : a = 3;
  int m : (V ? W : b) = 9;	// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
  int n : (V ? W : b) { 10 };	// { dg-warning "default member initializers for bit-fields only available with" "" { target c++17_down } }
  int o : 1 || new int { 0 };
};
#if __cplusplus >= 201402L
static_assert (U<true, 12>{}.j == 7);
static_assert (U<true, 13>{}.k == 8);
static_assert (U<true, 10>{}.l == 0);
static_assert (U<true, 11>{}.m == 9);
static_assert (U<true, 8>{}.n == 10);
static_assert (U<true, 7>{}.o == 0);
#endif
S s;
U<true, 10> u;

int
main ()
{
  if (s.c != 1 || s.d != 2 || s.e != 0 || s.f != 4 || s.g != 5 || s.h != 0)
    abort ();
  s.c = 47;		// { dg-warning "overflow in conversion from" }
  s.d = 47 * 2;		// { dg-warning "overflow in conversion from" }
  s.e = 47 * 4;		// { dg-warning "overflow in conversion from" }
  s.f = 47 * 8;		// { dg-warning "overflow in conversion from" }
  s.g = 47 * 16;	// { dg-warning "overflow in conversion from" }
  s.h = 2;		// { dg-warning "overflow in conversion from" }
  if (s.c != 15 || s.d != 15 * 2 || s.e != 15 * 4 || s.f != 15 * 8 || s.g != 15 * 16 || s.h != 0)
    abort ();
  if (u.j != 7 || u.k != 8 || u.l != 0 || u.m != 9 || u.n != 10 || u.o != 0)
    abort ();
  u.j = 47 * 32;	// { dg-warning "overflow in conversion from" }
  u.k = 47 * 32;	// { dg-warning "overflow in conversion from" }
  u.l = 47 * 4;		// { dg-warning "overflow in conversion from" }
  u.m = 47 * 32;	// { dg-warning "overflow in conversion from" }
  u.n = 47 * 32;	// { dg-warning "overflow in conversion from" }
  u.o = 2;		// { dg-warning "overflow in conversion from" }
  if (u.j != 15 * 32 || u.k != 15 * 32 || u.l != 15 * 4 || u.m != 15 * 32 || u.n != 15 * 32 || u.o != 0)
    abort ();
  s.c = 15;
  s.d = 15 * 2;
  s.e = 15 * 4;
  s.f = 16 * 8;
  s.g = 15 * 16;
  u.j = 15 * 32;
  u.k = 15 * 32;
  u.l = 15 * 4;
  u.m = 15 * 32;
  u.n = 15 * 32;
}
