// Build don't run:
// GROUPS passed templates membertemplates
extern "C" int printf(const char*, ...);

template <class X>
struct S
{
  template <class U>
  void g(U u)
  { printf ("In S::g(U)\n"); }

  int c[16];
};


template <class X>
struct T : public S<X>
{
  template <class U>
  void f(U u)
  { printf ("In T::f(U)\n"); g(u); }
};

int main()
{
  T<char*> t;
  t.f(3);
  t.f("adf");
}
