// { dg-do run  }
struct A { virtual void f() { } };
struct B { virtual void g() { } };
struct C : public A, public B { };

int main ()
{
  C* cp = 0;
  B* bp = 0;

  if (dynamic_cast <B*> (cp) != 0)
    return 1;

  if (dynamic_cast <void *> (bp) != 0)
    return 1;

  if (dynamic_cast <C*> (bp) != 0)
    return 1;
}
