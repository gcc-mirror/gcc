// PR c++/71495
// { dg-do compile { target c++11 } }

struct A;
template <class T> void f(T);	// { dg-bogus "initializing" }
template <class T> T&& declval();
struct B
{
  template <class T, class U> static decltype(f<T>(declval<U>())) g(int);
  template <class T, class U> void g(...);
} b;

int main()
{
  b.g<A,A>(42);
}
