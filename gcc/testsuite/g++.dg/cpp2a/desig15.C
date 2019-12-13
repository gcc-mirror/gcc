// { dg-do run }
// { dg-options "-pedantic" }

struct A { int a; };
struct B { int b; A c; int d; };
A a = { 1 };
B c = { 3, { 4 }, 5 };
#if __cpp_designated_initializers >= 201707L
A b = { .a = 2 };
B d = { .b = 6, .c { 7 }, .d = 8 };
B e = { .c = { .a = 9 } };
#else
A b = { 2 };
B d = { 6, { 7 }, 8 };
B e = { 0, { 9 } };
#endif

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
