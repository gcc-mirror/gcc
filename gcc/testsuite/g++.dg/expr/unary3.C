// PR c++/41131
// { dg-do compile }

struct X { enum E { a = 100 }; };

int
main ()
{
  X x;
  (void) &x.a;    // { dg-error "lvalue required" }
}
