// Build don't run:
// GROUPS passed templates membertemplates
extern "C" int printf(const char*, ...);

template <class X>
struct S
{
  template <class U>
  void f(U u) { printf ("%d\n", sizeof (U)); }

  int i[4];
};


int main()
{
  S<char*> s;
  s.f(3);
  s.f(s);
}
