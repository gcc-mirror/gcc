// { dg-do link  }
// GROUPS passed templates membertemplates
extern "C" int printf(const char*, ...);

struct S
{
  template <class U>
  void g(U u)
  { this; }
};

int main()
{
  S s;
  s.g(3);
}
