// Build don't run:
// GROUPS passed templates

template <class U>
struct S 
{
  template <class T>
  void foo(T t);

  template <class T>
  void bar(T t) { this->template foo<U>(3.74); }
};

template <>
template <>
void S<int>::foo(int) { }

int main()
{
  S<int> s;
  s.bar(3);
}
