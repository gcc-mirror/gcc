// { dg-do run { target c++17 } }
// { dg-options "-pedantic" }

struct S { int a; union { int b; double c; union { short e; long f; }; }; int d; };
S s = { 1, 2, 3 };
S t = { .a = 4, .b = 5, .d = 6 };	// { dg-warning "designated initializers only available with" "" { target c++17_down } }
S u = { .a = 7, .c = 8.0, .d = 9 };	// { dg-warning "designated initializers only available with" "" { target c++17_down } }
S v = { .c = 10.0, .d = 11 };		// { dg-warning "designated initializers only available with" "" { target c++17_down } }
S w = { .b = 12 };			// { dg-warning "designated initializers only available with" "" { target c++17_down } }
S x = { .b = 13 };			// { dg-warning "designated initializers only available with" "" { target c++17_down } }
S y = { .a = 14, .e = 15, .d = 16 };	// { dg-warning "designated initializers only available with" "" { target c++17_down } }
S z = { .f = 17 };			// { dg-warning "designated initializers only available with" "" { target c++17_down } }

int
main ()
{
  if (s.a != 1 || s.b != 2 || s.d != 3
      || t.a != 4 || t.b != 5 || t.d != 6
      || u.a != 7 || u.c != 8.0 || u.d != 9
      || v.a != 0 || v.c != 10.0 || v.d != 11
      || w.a != 0 || w.b != 12 || w.d != 0
      || x.a != 0 || x.b != 13 || x.d != 0
      || y.a != 14 || y.e != 15 || y.d != 16
      || z.a != 0 || z.f != 17 || z.d != 0)
    __builtin_abort ();
  return 0;
}
