/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-w -O2" } */

void fancy_abort(char *, int, const char *) __attribute__((__noreturn__));
template <unsigned N, typename> struct poly_int_pod { int coeffs[N]; };
template <unsigned N, typename> class poly_int : public poly_int_pod<N, int> {
public:
  template <typename Ca> poly_int &operator+=(const poly_int_pod<N, Ca> &);
};
template <unsigned N, typename C>
template <typename Ca>
poly_int<N, C> &poly_int<N, C>::operator+=(const poly_int_pod<N, Ca> &a) {
  for (int i = 0; i < N; i++)
    this->coeffs[i] += a.coeffs[i];
  return *this;
}
template <unsigned N, typename Ca, typename Cb>
poly_int<N, long> exact_div(poly_int_pod<N, Ca>, Cb) {
  poly_int<N, long> r;
  return r;
}
struct vec_prefix {
  unsigned m_num;
};
struct vl_ptr;
struct va_heap {
  typedef vl_ptr default_layout;
};
template <typename, typename A, typename = typename A::default_layout>
struct vec;
template <typename T, typename A> struct vec<T, A, int> {
  T &operator[](unsigned);
  vec_prefix m_vecpfx;
  T m_vecdata[];
};
template <typename T, typename A> T &vec<T, A, int>::operator[](unsigned ix) {
  m_vecpfx.m_num ? fancy_abort("", 9, __FUNCTION__), 0 : 0;
  return m_vecdata[ix];
}
template <typename T> struct vec<T, va_heap> {
  T &operator[](unsigned ix) { return m_vec[ix]; }
  vec<T, va_heap, int> m_vec;
};
class auto_vec : public vec<poly_int<2, long>, va_heap> {};
template <typename> class vector_builder : public auto_vec {};
class int_vector_builder : public vector_builder<int> {
public:
  int_vector_builder(poly_int<2, long>, int, int);
};
bool vect_grouped_store_supported() {
  int i;
  poly_int<2, long> nelt;
  int_vector_builder sel(nelt, 2, 3);
  for (i = 0; i < 6; i++)
    sel[i] += exact_div(nelt, 2);
}

