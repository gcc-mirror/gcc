// PR c++/51813
// { dg-require-visibility "" }
// { dg-options -fvisibility=hidden }
// { dg-final { scan-assembler-not "hidden\\s+_ZN1N1fI1AEEvT" } }

struct A { };
namespace N __attribute((visibility("default"))) {
  template <class T> void f(T) { }
  extern template void f(A);
}

int main()
{
  N::f(A());
}
