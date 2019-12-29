// PR c++/92745 - bogus error when initializing array of vectors.
// { dg-do compile { target c++11 } }
// { dg-options "-Wno-psabi -w" }

template <typename a, int b> struct c {
  typedef a d[b];
};

template <typename a, int b> struct array {
  typename c<a, b>::d e;
  a operator[](long);
};

template<class T>
using vec4_t __attribute__((vector_size(4*sizeof(T)))) = float;

array<vec4_t<float>, 4>
transpose(array<vec4_t<float>, 4> col)
{
  array<vec4_t<float>, 4>
    ret{vec4_t<float>{col[0][0], col[1][0], col[2][0], col[3][0]},
        vec4_t<float>{col[0][1], col[1][1], col[2][1], col[3][1]},
        vec4_t<float>{col[0][2], col[1][2], col[2][2], col[3][2]},
        vec4_t<float>{col[0][3], col[1][3], col[2][3], col[3][3]}};
  return ret;
}
