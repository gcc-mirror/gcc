/* PR c++/99108 */
/* { dg-require-ifunc "" }  */

struct A {
  template <class T>
  void foo(T);
};
template <class T>
void A::foo(T)
{
  int f(void) __attribute__((target("default")));
  int f(void) __attribute__((target("arch=atom")));
  int b = f();
}
void bar(void)
{
  A c;
  c.foo(7);
}
