// Build don't run:
// GROUPS passed templates membertemplates
extern "C" int printf(const char*, ...);

struct S {
  template <class T, class U>
  S(T, U, T);
};


template <class T, class U>
S::S(T t1, U u1, T t2)
{
  printf("Hello, world.\n");
}


int main()
{
  S s1(3, "abc", 3);
  S s2('a', s1, 'a');
}
