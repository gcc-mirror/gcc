// Build don't run:
// GROUPS passed templates membertemplates
extern "C" int printf(const char*, ...);

template <class X>
struct S
{
  template <class U>
  void f(U u)
  { printf ("In S::f(U)\n"); g(u); }

  template <class U>
  void g(U u)
  { printf ("In S::g(U)\n"); }

  int c[16];
};

int main()
{
  S<char*> s;
  s.f(3);
  s.f("adf");
}
