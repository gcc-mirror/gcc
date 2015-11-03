template <int N>
struct S
{
  int s;
  #pragma omp declare simd notinbranch
  int f0 (int x);
  #pragma omp declare simd notinbranch uniform(this)
  int f1 (int x);
  #pragma omp declare simd notinbranch linear(this:sizeof(this)/sizeof(this))
  int f2 (int x);
};

template <int N>
struct T
{
  int t[64];
  #pragma omp declare simd aligned(this:32) uniform(this) linear(x)
  int f3 (int x);
};
