// { dg-do run { target c++11 } }
// { dg-options "" }

extern "C" void abort ();

int a;
struct A {
  A() { ++a; }
  A(int);
  A(const A&);
  ~A() { --a; }
};

struct B {
  A a1;
  A a2;
};

int main()
{
  {
    B b = { A(), A(({goto out; 42;})) };
  }
 out:
  if (a != 0)
    abort ();
}
