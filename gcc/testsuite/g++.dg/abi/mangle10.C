// { dg-options "-fabi-version=0" }

template <template <typename> class Q>
void f (typename Q<int>::X) {}

template <typename Q>
struct S {
  typedef int X;
};

template void f<S> (int);

// { dg-final { scan-assembler _Z1fI1SEvNT_IiE1XE } }
