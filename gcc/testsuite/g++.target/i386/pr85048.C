/* PR target/85048 */
/* { dg-do compile }  */
/* { dg-options "-std=c++17 -O2 -mavx512vl -mavx512dq -mprefer-vector-width=512" } */
/* { dg-final { scan-assembler-times {(?n)vcvtudq2pd[ \t]+} 2 } } */
/* { dg-final { scan-assembler-times {(?n)vcvttps2udq[ \t]+} 2 } } */
/* { dg-final { scan-assembler-times {(?n)vcvttpd2udqy?[ \t]+} 1 } } */

#include <cstdint>

template <class T, int N, int Size = N * sizeof(T)>
using V [[gnu::vector_size(Size)]] = T;

template <class From, class To> V<To, 4> cvt4(V<From, 4> x) {
    return V<To, 4>{To(x[0]), To(x[1]), To(x[2]), To(x[3])};
}
template <class From, class To> V<To, 8> cvt8(V<From, 8> x) {
    return V<To, 8>{
        To(x[0]), To(x[1]), To(x[2]), To(x[3]),
        To(x[4]), To(x[5]), To(x[6]), To(x[7])
    };
}

#define _(name, from, to, size) \
auto name(V<from, size> x) { return cvt##size<from, to>(x); }
// integral -> double
_(vcvtudq2pd, uint32_t, double, 4)
_(vcvtudq2pd, uint32_t, double, 8)

_( cvttps2udq, float, uint32_t,  4)
_(vcvttps2udq, float, uint32_t,  8)

// double -> integral
_(vcvttpd2udq, double, uint32_t, 4)
