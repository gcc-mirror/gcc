// { dg-do run  }
// { dg-options "-ansi -pedantic-errors -w" }
struct S 
{
  template <class U>
  struct Y {
    template <class T>
    void foo(T t);
  };
};

template <>
template <>
void S::Y<char>::foo<int>(int i) { }

int main()
{
  S::Y<char> s;
  s.foo<int>(3.0);
}
