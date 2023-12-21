// PR c++/38600
// { dg-final { scan-assembler "_Z3barIiE1AIX3fooIT_EEEv" } }
// { dg-additional-options -fabi-compat-version=0 }

template<void (*)()> struct A {};

template<typename> void foo();

template<typename T> A<foo<T> > bar();

void baz()
{
  bar<int>();
}
