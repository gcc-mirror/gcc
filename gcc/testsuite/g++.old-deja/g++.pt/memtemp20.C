// { dg-do link  }
// GROUPS passed templates membertemplates
extern "C" int printf(const char*, ...);

struct S
{
  template <class T, class U>
  void foo(T t, U u);

  template <class U>
  void foo(char*, U);

  void foo(int i);
};

template <class T, class U>
void S::foo(T t, U u)
{
  printf ("T,U version\n");
}


template <class U>
void S::foo(char*, U u)
{
  printf ("char*,U version\n");
}


void S::foo(int i)
{
  printf ("int version\n");
}


int main()
{
  S s;
  s.foo(3);
  s.foo(3, 3);
  s.foo("abc", s);
}

