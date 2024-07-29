/* { dg-do compile { target { struct_musttail } } } */
/* { dg-require-effective-target external_musttail } */
/* A lot of architectures will not build this due to PR115606 and PR115607 */
/* { dg-options "-std=gnu++11" } */
/* { dg-additional-options "-fdelayed-branch" { target sparc*-*-* } } */

class Foo {
public:
  int a, b;
  Foo(int a, int b) : a(a), b(b) {}
};

Foo __attribute__((noinline,noclone,noipa))
callee (int i)
{
  return Foo(i, i+1);
}

Foo __attribute__((noinline,noclone,noipa))
caller (int i)
{
  [[gnu::musttail]] return callee (i + 1);
}

template<typename T>
T __attribute__((noinline,noclone,noipa)) foo (T i)
{
  return i + 1;
}

int
caller2 (int k)
{
  [[gnu::musttail]] return foo<int>(1);
}

template<typename T>
T caller3 (T v)
{
  [[gnu::musttail]] return foo<T>(v);
}

int call3(int i)
{
  [[gnu::musttail]] return caller3<int>(i + 1);
}

struct Bar {
  int a;
  Bar(int a) : a(a) {}
  Bar operator+(Bar o) { return Bar(a + o.a); }
};

#if __OPTIMIZE__ >= 1
Bar
caller4 (Bar k)
{
  [[gnu::musttail]] return caller3<Bar>(Bar(99));
}
#endif
