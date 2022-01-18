// PR c++/104055
// { dg-do run { target c++20 } }

int g;

struct A { 
  ~A () { if (a != 17 || b != 26) __builtin_abort (); g = 42; }
  consteval A () : a (17), b (26) {}
  int a, b;
};

int
main ()
{
  A{};
  if (g != 42)
    __builtin_abort ();
}
