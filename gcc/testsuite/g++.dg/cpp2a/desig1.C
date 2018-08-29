// { dg-do compile }
// { dg-options "-pedantic" }

struct A { int a; };
struct B { int b; A c; int d; };
A a = { 1 };
A b = { .a = 2 };			// { dg-warning "designated initializers only available with" "" { target c++17_down } }
B c = { 3, { 4 }, 5 };
B d = { .b = 6, .c { 7 }, .d = 8 };	// { dg-warning "designated initializers only available with" "" { target c++17_down } }
B e = { .c = { .a = 9 } };		// { dg-warning "designated initializers only available with" "" { target c++17_down } }

int
main ()
{
  if (a.a != 1 || b.a != 2
      || c.b != 3 || c.c.a != 4 || c.d != 5
      || d.b != 6 || d.c.a != 7 || d.d != 8
      || e.b != 0 || e.c.a != 9 || e.d != 0)
    __builtin_abort ();
  return 0;
}
