// PR c++/79288
// { dg-do compile { target nonpic } }
// { dg-require-effective-target tls }
// { dg-options "-O2" }
// { dg-final { scan-assembler-not "@tpoff" { target i?86-*-* x86_64-*-* } } }

struct S
{
  static __thread int *p;
};

template <int N>
struct T
{
  static __thread int *p;
};

int *
foo ()
{
  return S::p;
}

int *
bar ()
{
  return T<0>::p;
}
