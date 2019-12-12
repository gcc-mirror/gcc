// PR c++/60702

extern "C" void abort ();
struct S { S () { i = 42; }; int i; };
thread_local S s1, s2, s3, s4;
struct T { static thread_local S u1, u2, u3, u4, u5, u6, u7, u8; int i; } t;
thread_local S T::u1, T::u2, T::u3, T::u4, T::u5, T::u6, T::u7, T::u8;

S *f1 () { return &s1; }
int *f2 () { return &s2.i; }
S *f3 () { return &t.u1; }
int *f4 () { return &t.u2.i; }
S *f5 () { return &T::u3; }
int *f6 () { return &T::u4.i; }
template <int N>
S *f7 () { return &s3; }
template <int N>
int *f8 () { return &s4.i; }
template <int N>
S *f9 () { return &t.u5; }
template <int N>
int *f10 () { return &t.u6.i; }
template <int N>
S *f11 () { return &T::u7; }
template <int N>
int *f12 () { return &T::u8.i; }
