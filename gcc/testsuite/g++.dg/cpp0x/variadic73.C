// { dg-do "run" }
// { dg-options "-std=gnu++0x" }
struct A {};
struct B {};
struct C {};

template<typename... Exceptions> void f(int idx) throw(Exceptions...) {
  if (idx == 0) throw A();
  else if (idx == 1) throw B();
  else if (idx == 2) throw C();
}

extern "C" void abort();

int main()
{
  try {
    f<A, B, C>(0);
    abort();
  } catch (A) {
  }
  try {
    f<A, B, C>(1);
    abort();
  } catch (B) {
  }
  try {
    f<A, B, C>(2);
    abort();
  } catch (C) {
  }
  return 0;
}
