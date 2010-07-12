// PR c++/35758
// { dg-do compile }
// { dg-options "-msse" { target { { i?86-*-* x86_64-*-* } && ilp32 } } }
// { dg-require-effective-target sse }
// Ignore warning on some powerpc-linux configurations.
// { dg-prune-output "non-standard ABI extension" }
// { dg-prune-output "mangled name" }

#define vector __attribute__((vector_size(16)))

template<int N> vector signed int foo (vector float value) {}

template<int> void foo (float) {}

int
main ()
{
  vector float v;
  float f;
  foo<1> (v);
  foo<1> (f);
}
