extern "C" void abort();

struct A
{
  enum { a = 3}* p;
  int f() { return (int) a; }
};

int main()
{
  A a;

  if (a.f() != 3)
    abort();
}
