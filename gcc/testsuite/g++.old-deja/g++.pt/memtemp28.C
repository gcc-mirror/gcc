// { dg-do run  }
extern "C" void abort();

int k;

template <class X>
struct S
{
  template <class U>
  void f(U u)
  { ++k; g(u); }

  template <class U>
  void g(U u)
  { ++k; }

  int c[16];
};

int main()
{
  S<char*> s;
  s.f(3);
  s.f("adf");

  if (k != 4)
    abort();
}
