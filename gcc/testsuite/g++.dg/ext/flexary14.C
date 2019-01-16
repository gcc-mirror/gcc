// PR c++/69349 - template substitution error for flexible array members
// { dg-do compile }

template <class>
struct A;

template <class T>
struct A<T[]> { typedef int X; };

template <class T> int foo (T&, typename A<T>::X = 0) { return 0; }

struct B {
  int n, a[];     // { dg-error "10:ISO C\\+\\+ forbids flexible array member" }
};

void bar (B *b)
{
    foo (b->a);
}
