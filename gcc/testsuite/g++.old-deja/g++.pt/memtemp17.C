// Build don't run:
// GROUPS passed templates membertemplates
extern "C" int printf(const char*, ...);

template <class V>
struct S {
  template <class T, class U>
  S(T, U, T);
};


template <class V>
template <class T, class U>
S<V>::S(T t1, U u1, T t2)
{
  printf("Hello, world.\n");
}


int main()
{
  S<int> s1(3, "abc", 3);
  S<int> s2('a', s1, 'a');

  S<char> s3("abc", 3, "abc");
}
