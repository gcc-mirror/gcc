/* PR target/112824 */
/* { dg-do compile } */
/* { dg-options "-std=c++23 -O3 -march=skylake-avx512 -mprefer-vector-width=512" } */
/* { dg-final { scan-assembler-not "vmov(?:dqu|apd)\[ \\t\]+\[^\n\]*%ymm" } } */

#include <bit>
#include <concepts>
#include <cstddef>
#include <cstdint>

template <ptrdiff_t W, typename T>
using Vec [[gnu::vector_size(W * sizeof(T))]] = T;

// Omitted: 16 without AVX, 32 without AVX512F,
// or for forward compatibility some AVX10 may also mean 32-only
static constexpr ptrdiff_t VectorBytes = 64;
template<typename T>
static constexpr ptrdiff_t VecWidth = 64 <= sizeof(T) ? 1 : 64/sizeof(T);

template <typename T, ptrdiff_t N> struct Vector{
    static constexpr ptrdiff_t L = N;
    T data[L];
    static constexpr auto size()->ptrdiff_t{return N;}
};
template <std::floating_point T, ptrdiff_t N> struct Vector<T,N>{
    static constexpr ptrdiff_t W = N >= VecWidth<T> ? VecWidth<T> : ptrdiff_t(std::bit_ceil(size_t(N))); 
    static constexpr ptrdiff_t L = (N/W) + ((N%W)!=0);
    using V = Vec<W,T>;
    V data[L];
    static constexpr auto size()->ptrdiff_t{return N;}
};
/// should be trivially copyable
/// codegen is worse when passing by value, even though it seems like it should make
/// aliasing simpler to analyze?
template<typename T, ptrdiff_t N>
[[gnu::always_inline]] constexpr auto operator+(Vector<T,N> x, Vector<T,N> y) -> Vector<T,N> {
    Vector<T,N> z;
    for (ptrdiff_t n = 0; n < Vector<T,N>::L; ++n) z.data[n] = x.data[n] + y.data[n];
    return z;
}
template<typename T, ptrdiff_t N>
[[gnu::always_inline]] constexpr auto operator*(Vector<T,N> x, Vector<T,N> y) -> Vector<T,N> {
    Vector<T,N> z;
    for (ptrdiff_t n = 0; n < Vector<T,N>::L; ++n) z.data[n] = x.data[n] * y.data[n];
    return z;
}
template<typename T, ptrdiff_t N>
[[gnu::always_inline]] constexpr auto operator+(T x, Vector<T,N> y) -> Vector<T,N> {
    Vector<T,N> z;
    for (ptrdiff_t n = 0; n < Vector<T,N>::L; ++n) z.data[n] = x + y.data[n];
    return z;
}
template<typename T, ptrdiff_t N>
[[gnu::always_inline]] constexpr auto operator*(T x, Vector<T,N> y) -> Vector<T,N> {
    Vector<T,N> z;
    for (ptrdiff_t n = 0; n < Vector<T,N>::L; ++n) z.data[n] = x * y.data[n];
    return z;
}

template <typename T, ptrdiff_t N> struct Dual {
  T value;
  Vector<T, N> partials;
};
// Here we have a specialization for non-power-of-2 `N`
template <typename T, ptrdiff_t N> 
requires(std::floating_point<T> && (std::popcount(size_t(N))>1))
struct Dual<T,N> {
  Vector<T, N+1> data;
};

template<ptrdiff_t W, typename T>
consteval auto firstoff(){
    static_assert(std::same_as<T,double>, "type not implemented");
    if constexpr (W==2) return Vec<2,int64_t>{0,1} != 0;
    else if constexpr (W == 4) return Vec<4,int64_t>{0,1,2,3} != 0;
    else if constexpr (W == 8) return Vec<8,int64_t>{0,1,2,3,4,5,6,7} != 0;
    else static_assert(false, "vector width not implemented");
}

template <typename T, ptrdiff_t N>
[[gnu::always_inline]] constexpr auto operator+(Dual<T, N> a,
                                                Dual<T, N> b)
  -> Dual<T, N> {
  if constexpr (std::floating_point<T> && (std::popcount(size_t(N))>1)){
    Dual<T,N> c;
    for (ptrdiff_t l = 0; l < Vector<T,N>::L; ++l)
      c.data.data[l] = a.data.data[l] + b.data.data[l]; 
    return c;
  } else return {a.value + b.value, a.partials + b.partials};
}

template <typename T, ptrdiff_t N>
[[gnu::always_inline]] constexpr auto operator*(Dual<T, N> a,
                                                Dual<T, N> b)
  -> Dual<T, N> {
  if constexpr (std::floating_point<T> && (std::popcount(size_t(N))>1)){
    using V = typename Vector<T,N>::V;
    V va = V{}+a.data.data[0][0], vb = V{}+b.data.data[0][0];
    V x = va * b.data.data[0];
    Dual<T,N> c;
    c.data.data[0] = firstoff<Vector<T,N>::W,T>() ? x + vb*a.data.data[0] : x;
    for (ptrdiff_t l = 1; l < Vector<T,N>::L; ++l)
      c.data.data[l] = va*b.data.data[l] + vb*a.data.data[l]; 
    return c;
  } else return {a.value * b.value, a.value * b.partials + b.value * a.partials};
}

void prod(Dual<Dual<double,7>,2> &c, const Dual<Dual<double,7>,2> &a, const Dual<Dual<double,7>,2>&b){
    c = a*b;
}
