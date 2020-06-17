// { dg-do run }
// { dg-additional-options -fstrong-eval-order=all }

struct A
{
  virtual A& operator=(const A&) { return *this; }
};

int i;

A& f() { if (i != 1) __builtin_abort (); i = 2; static A a; return a; }
A& g() { if (i != 0) __builtin_abort (); i = 1; static A a; return a; }

int main()
{
  f() = g();
  if (i != 2) __builtin_abort ();
}
