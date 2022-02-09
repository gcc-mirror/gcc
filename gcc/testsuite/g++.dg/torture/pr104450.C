// { dg-do compile }
// { dg-additional-options "-fnon-call-exceptions" }
// { dg-additional-options "-mavx512f" { target x86_64-*-* i?86-*-* } }

#define vectsize 64
typedef int __attribute__((__vector_size__ (vectsize))) V;
typedef float __attribute__((__vector_size__ (vectsize))) F;
F f;
V v;
struct g{~g();};
void
foo (void)
{
  g t;
  v += (V) (0 <= f);
}
