// { dg-do link  }
// GROUPS passed templates membertemplates
extern "C" int printf(const char*, ...);

struct S {
  template <class T>
  operator T();
};

template <class T>
S::operator T()
{
  printf("Hello, world.\n");
  return T();
}


template S::operator int();

int main()
{
  S s;

  int i = s.operator int();
}
