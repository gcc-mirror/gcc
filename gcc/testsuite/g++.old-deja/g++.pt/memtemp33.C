// Build don't run:
// GROUPS passed templates membertemplates
extern "C" int printf(const char*, ...);

template <class T>
struct S
{
  template <class U>
  void g(U u)
  { i; }

  int i;
};

int main()
{
  S<char> s;
  s.g(3);
}
