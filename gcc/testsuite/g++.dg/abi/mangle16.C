// { dg-options "-fabi-version=0" }

enum E { e = 3 };

template <int I> struct S {};

template <int I> void f (S<e + 1>) {}
template void f<7>(S<e + 1>);

template <int I> void g (S<e>) {}
template void g<7>(S<e>);

// { dg-final { scan-assembler _Z1fILi7EEv1SIXplL1E3ELi1EEE } }
// { dg-final { scan-assembler _Z1gILi7EEv1SIL1E3EE } }
