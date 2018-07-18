// { dg-do run  }
// Test that we allow simple throw specs on pointers.

void f() throw () { }
void (*pf)() throw () = f;

struct A
{
  void g() throw () { }
  static void (A::*pmf)() throw ();
};

void (A::* A::pmf)() throw() = &A::g;

int main()
{
  pf ();
  A a;
  (a.*A::pmf)();
}
