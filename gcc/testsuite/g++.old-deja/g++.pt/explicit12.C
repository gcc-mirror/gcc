// Build don't run:
// Special g++ Options: -ansi -pedantic-errors -w
// GROUPS passed templates
template <class U>
struct S 
{
  template <class T>
  void foo(T t);
};


template <>
template <>
void S<char*>::foo<int>(int) {}

int main()
{
  S<char*> s;
  s.template foo<int>(3.0);
}
