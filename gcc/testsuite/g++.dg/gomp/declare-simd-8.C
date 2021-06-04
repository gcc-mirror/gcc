// PR c++/100872

template <int N, typename T>
struct S {
  #pragma omp declare simd aligned(a : N * 2) aligned(b) linear(ref(b): N)
  float foo (float *a, T *&b) { return *a + *b; }
};

S<16, float> s;

float
bar (float *a, float *p)
{
  return s.foo (a, p);
}
