// { dg-do run }

extern "C" void abort ();

struct A
{
  ~A();
};

A::~A () {
  abort ();
}

struct B
{
  ~B();
};

B::~B () {
  if(true) return;
  A a;
}

int main()
{
  B b;
  return 0;
}
