// { dg-options "-fabi-version=0" }

enum E { e = 3 };

template <int I> struct S {};

template <int I> void f (S<e + 1>) {}
template void f<7>(S<e + 1>);

template <int I> void g (S<e>) {}
template void g<7>(S<e>);

template <int I> void h (S<I + 1>) {}
template void h<7>(S<7 + 1>);

// { dg-final { scan-assembler _Z1fILi7EEv1SILi4EE } }
// { dg-final { scan-assembler _Z1gILi7EEv1SILi3EE } }
// { dg-final { scan-assembler _Z1hILi7EEv1SIXplT_Li1EEE } }
