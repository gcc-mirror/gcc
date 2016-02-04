// { dg-do compile { target c++11 } }

#define SA(x) static_assert ((x), #x)

struct X { int a, b; };

void
foo ()
{
  SA (&X::a);
  SA (&X::a == &X::a);
  SA (!(&X::a != &X::a));
  SA (&X::a != &X::b);
  SA (!(&X::a == &X::b));
  SA ((!&X::b) == 0);
  SA (!(&X::b == 0));
}
