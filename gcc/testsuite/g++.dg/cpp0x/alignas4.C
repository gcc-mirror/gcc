// PR c++/59012
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "align 8" { target { { i?86-*-* x86_64-*-* } && { { ! ia32 } && { ! *-*-darwin* } } } } } }
// { dg-final { scan-assembler "align 4" { target { ia32 && { ! *-*-darwin* } } } } }

// Darwin produces aligned .zerofill directives for these.
// { dg-final { scan-assembler {zerofill[^\n\r]+_a,4,2} { target { ilp32 && *-*-darwin* } } } }
// { dg-final { scan-assembler {zerofill[^\n\r]+_a,8,3} { target { lp64 && *-*-darwin* } } } }
// { dg-final { scan-assembler {zerofill[^\n\r]+_a2,4,2} { target { ilp32 && *-*-darwin* } } } }
// { dg-final { scan-assembler {zerofill[^\n\r]+_a2,8,3} { target { lp64 && *-*-darwin* } } } }

template <class... T>
struct A
{
  alignas(T...) char t;
};

A<int,double> a;

template <class... T>
struct A2
{
  [[gnu::aligned (alignof (T))...]] char t;
};

A2<int,double> a2;
