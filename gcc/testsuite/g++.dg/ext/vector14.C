// PR c++/35758
// { dg-do compile }

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
