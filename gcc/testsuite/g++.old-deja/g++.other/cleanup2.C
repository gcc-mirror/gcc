// Origin: Bryan Scattergood <bryan@fsel.com>
// Special g++ Options: -O -fno-exceptions -w

extern "C" void abort();

class A
{
public:
  A();
  ~A();
  int foo();
};

A::A() {}
A::~A() { abort (); }
int A::foo() {}

extern int f()
{
  return 0;
}

int main()
{
  return ((f() != 0) ? A().foo() : 0);
}
