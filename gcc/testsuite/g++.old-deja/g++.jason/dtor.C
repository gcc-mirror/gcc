// { dg-do assemble  }
struct A {
  ~A();
};

struct B {
  ~B();
};

int main()
{
  A a;
  a.~B();			// { dg-error "" } wrong name
}
