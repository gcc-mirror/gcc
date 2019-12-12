/* { dg-do compile } */
template <typename, typename> struct a;
template <template <typename> class b, typename c, typename f, typename... d>
struct a<b<f, d...>, c> {
  using e = b<c>;
};
template <typename f> class h {
public:
  typedef f g;
};
template <typename j, typename c> using k = typename a<j, c>::e;
template <typename j> struct l { template <typename f> using m = k<j, f>; };
template <typename j> struct n {
  typedef typename j::g o;
  template <typename f> struct p {
    typedef typename l<j>::template m<f> other;
  };
};
template <typename f, typename j> struct F {
  typedef typename n<j>::template p<f>::other q;
};
template <typename f, typename j = h<f>> class r {
public:
  typename n<typename F<f, j>::q>::o operator[](long);
  f *t() noexcept;
};
class s {
  void m_fn2();
  r<int (s::*)()> u;
};
void s::m_fn2() try {
  for (int i;;)
    (this->*u[i])();
} catch (...) {
}
