// Special g++ Options: -ansi -pedantic-errors -w
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
  s.template foo<int>(3.0);
}
