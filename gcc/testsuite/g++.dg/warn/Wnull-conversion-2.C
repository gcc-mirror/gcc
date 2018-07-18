// { dg-do compile }
// { dg-options "-Wconversion-null" }

#include <stddef.h>

class Foo {
 public:
  template <typename T1, typename T2>
  static void Compare(const T1& expected, const T2& actual) { }

  template <typename T1, typename T2>
  static void Compare(const T1& expected, T2* actual) { }

};

template<typename T1>
class Foo2 {
 public:
  Foo2(int x);
  template<typename T2> void Bar(T2 y);
};

template<typename T3> void func(T3 x) { }

typedef Foo2<int> MyFooType;

void func1(long int a) {
  MyFooType *foo2 = new MyFooType(NULL); // { dg-warning "passing NULL to" }
  foo2->Bar(a);
  func(NULL);
  func<int>(NULL);                       // { dg-warning "passing NULL to" }
  func<int *>(NULL);
}

int x = 1;

int
main()
{
  int *p = &x;

  Foo::Compare(0, *p);
  Foo::Compare<long int, int>(NULL, p);  // { dg-warning "passing NULL to" }
  Foo::Compare(NULL, p);
  func1(NULL);                           // { dg-warning "passing NULL to" }

  return 0;
}
