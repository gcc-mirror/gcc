// Build don't link:
// GROUPS passed templates
// Special g++ Options: -ansi -pedantic-errors -w
template <class T, class U>
void foo(U u, T t);

template <class T>
void foo(T t);

template <class T>
struct S {};

template <class T>
void foo(const S<T>&);

void bar()
{
  void (*fn)(double, int) = 
    (void (*)(double, int)) &foo<int>;
  void (*fn2)(double) = foo<double>;
  foo<int>(3, 3.0);
  foo<int>(S<int>());
}
