// { dg-options "-std=gnu++11" }
typedef int T __attribute__((vector_size(2*sizeof(int))));

void foo(T& r, const T& a, const T& b)
{
  constexpr T c = a < b; // { dg-error "constant" }
  r = c ? a : b;
}
