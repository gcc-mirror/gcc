// Build don't run:
// GROUPS passed templates membertemplates
extern "C" const char* printf(const char*, ...);

template <class T>
struct S
{
  template <class U, class V>
  void foo(U, V);
};


template <class T>
template <class U, class V>
void S<T>::foo(U, V)
{
  printf("Hello, world.\n");
}


int main()
{
  S<int> s;
  s.foo(3, 3);
  s.foo("hello", s);

  S<char*> s2;
  s2.foo(3, 3);
  s2.foo("hello", s);
}
