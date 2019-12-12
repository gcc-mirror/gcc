// PR c++/90473 - wrong code with nullptr in default argument.
// { dg-do run { target c++11 } }

int g;
void f() { g++; }

void fn1 (void* p = (f(), nullptr)) { }
void fn2 (int p = (f(), 0)) { }

int main()
{
  fn1 ();
  if (g != 1)
    __builtin_abort ();
  fn2 ();
  if (g != 2)
    __builtin_abort ();
}
