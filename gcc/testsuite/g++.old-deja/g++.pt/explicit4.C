// { dg-do assemble  }
// GROUPS passed templates
template <class T>
void foo(T t);

template <class T>
struct S {};

template <class T>
void bar(T t)
{
  void (*f)(S<T> ) = &foo<S<T> >;
}


void baz()
{
  bar(3);
}
