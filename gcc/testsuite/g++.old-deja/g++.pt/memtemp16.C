// { dg-do link  }
// GROUPS passed templates membertemplates
extern "C" int printf(const char*, ...);

template <class U>
struct S {
  template <class T>
  void operator+(T);
};

template <class U>
template <class T>
void S<U>::operator+(T)
{
  printf("Hello, world.\n");
}


int main()
{
  S<int> s;
  s + 3;
  s + s;
  s.operator+("Hi");

  S<S<int> > s2;
  s2 + 3;
  s2 + s;
  s2.operator+("Hi");
}
