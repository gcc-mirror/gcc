// { dg-do compile }
// { dg-options "-O2" }

class A
{
 public:
  template <class T>
  T foo(T a) __attribute__ ((noinline));
};

template <class T>
  T A::foo(T a)
  {
    return a+1;
  }

int bar(A a) {
  return a.foo(1);
}

// { dg-final { scan-assembler "_ZN1A3fooIiEET_S1_" } }
