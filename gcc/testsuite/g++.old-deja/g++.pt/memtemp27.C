// { dg-do link  }
// GROUPS passed templates membertemplates
extern "C" int printf(const char*, ...);

template <class X>
struct S
{
  S()
  { printf ("In S::S()\n"); f(3); }
  
  S(char)
  { printf ("In S::S(char)\n"); f(*this); }

  template <class U>
  void f(U u)
  { printf ("In S::f(U)\nsizeof(U) == %d\n", sizeof(u)); }

  int c[16];
};

int main()
{
  S<char*> s;
  S<char*> s2('a');
}
