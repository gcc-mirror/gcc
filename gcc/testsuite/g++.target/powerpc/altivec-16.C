// PR c++/36662
// { dg-do compile }
// { dg-options "-maltivec" }
// { dg-require-effective-target powerpc_altivec }

#define vector __attribute__((altivec (vector__)))

template <typename c> struct S {};

template <> struct S<vector float>
{
  static vector float zero;
};

template <int>
void g (void)
{
  vector float t = S<vector float>::zero;
}
