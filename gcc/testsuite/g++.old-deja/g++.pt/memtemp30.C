// Build don't run:
// GROUPS passed templates membertemplates
extern "C" int printf(const char*, ...);

template <class X>
struct S
{
  template <class U>
  void g(U u) { this; }
};


int main()
{
  S<char*> s;
  s.g(3);
}
