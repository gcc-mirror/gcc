// { dg-do link  }
// GROUPS passed templates membertemplates
extern "C" int printf(const char*, ...);

struct S {
  template <class T>
  void foo(T);
};


template <class T>
void S::foo(T)
{
  printf("Hello, world.\n");
}



int main()
{
  S s;
  s.foo(3);
  s.foo(s);
}
