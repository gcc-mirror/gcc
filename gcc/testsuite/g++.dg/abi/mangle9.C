// { dg-options "-fabi-version=0 -fabi-compat-version=0" }

template <typename Q>
void f (typename Q::X) {}

struct S {
  typedef int X;
};

template void f<S> (int);

// { dg-final { scan-assembler _Z1fI1SEvNT_1XE } }
