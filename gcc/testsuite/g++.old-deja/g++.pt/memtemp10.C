// { dg-do link  }
// GROUPS passed templates membertemplates
extern "C" int printf(const char*, ...);

struct S {
  template <class T>
  operator T*();
};


template <class T>
S::operator T*()
{
  printf("Hello, world.\n");
  return 0;
}


int main()
{
  S s;

  char* cp = s.operator char*();
}
