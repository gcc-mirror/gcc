// Build don't run:
// GROUPS passed templates membertemplates
extern "C" int printf(const char*, ...);

struct S
{
  template <class U>
  S(U u, int i) {}

  template <class T>
  T foo(T t) 
  { 
    printf("Hello, world.\n");
    return t; 
  }
};


int main()
{
  S s(3, 4);
  int i = s.foo(3);
  s.foo("hello");
  s.foo(s);
  
  S s2("hello", 5);
}
