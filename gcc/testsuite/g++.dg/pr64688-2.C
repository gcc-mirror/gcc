// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-std=c++11 -O3 -march=westmere" }

template <int> struct int_ {};
template <typename> struct add_const { typedef int type; };
template <typename> struct add_reference { typedef int type; };
template <typename T> struct next { typedef typename T::next type; };
template <typename> struct size_impl;
template <typename T> struct msvc_eti_base : T {};
template <int N> struct long_ {
  static const int value = N;
  typedef long_<N + 1> next;
};
template <typename Sequence>
struct size : msvc_eti_base<typename size_impl<
typename Sequence::tag>::template apply<Sequence>> {};
template <typename Base> struct v_item : Base {
  typedef typename next<typename Base::size>::type size;
};
template <typename = int> struct vector0 {
  typedef int tag;
  typedef long_<0> size;
};
template <> struct size_impl<int> {
  template <typename Vector> struct apply : Vector::size {};
};
template <typename> struct vector3 : v_item<v_item<v_item<vector0<>>>> {};
template <typename> struct layout { typedef vector3<int> color_space_t; };
template <typename> struct kth_element_const_reference_type;
template <typename> struct iterator_adaptor_get_base;
template <typename, typename, int> struct homogeneous_color_base;
template <typename> struct element_const_reference_type;
template <typename Element, typename Layout>
  struct homogeneous_color_base<Element, Layout, 3> {
  Element _v0, _v1, _v2;
  typename element_const_reference_type<homogeneous_color_base>::type
    at(int_<0>) {
    return _v0;
  }
  typename element_const_reference_type<homogeneous_color_base>::type
    at(int_<1>) {
    return _v1;
  }
  typename element_const_reference_type<homogeneous_color_base>::type
    at(int_<2>) {
    return _v2;
  }
};
template <typename Element, typename Layout, int K1>
  struct kth_element_const_reference_type<
  homogeneous_color_base<Element, Layout, K1>>
  : add_reference<typename add_const<Element>::type> {};
template <int K, typename E, typename L, int N>
  typename add_reference<typename add_const<E>::type>::type
  at_c(homogeneous_color_base<E, L, N> p1) {
  return p1.at(int_<K>());
}
template <typename> class memory_based_step_iterator;
template <typename> class memory_based_2d_locator;
template <typename> class image_view;
template <typename, typename> struct pixel;
struct iterator_type_from_pixel {
  typedef pixel<unsigned char, layout<vector3<int>>> *type;
};
template <typename XIterator> struct type_from_x_iterator {
    typedef image_view<
    memory_based_2d_locator<memory_based_step_iterator<XIterator>>> view_t;
};
template <typename>
struct element_const_reference_type
: kth_element_const_reference_type<
homogeneous_color_base<unsigned, layout<int>, 3>> {};
template <typename, typename>
  struct pixel : homogeneous_color_base<unsigned char, layout<int>,
  size<layout<int>::color_space_t>::value> {
};
template <typename Iterator>
struct iterator_adaptor_get_base<memory_based_step_iterator<Iterator>> {
  typedef Iterator type;
};
template <typename> class memory_based_2d_locator {
 public:
    typedef iterator_adaptor_get_base<memory_based_step_iterator<
      pixel<unsigned, layout<vector3<int>>> *>>::type x_iterator;
};
template <typename> class image_view {
 public:
  typedef memory_based_2d_locator<int>::x_iterator x_iterator;
  x_iterator row_begin___trans_tmp_2;
  x_iterator row_begin(int) { return row_begin___trans_tmp_2; }
};
template <typename, bool, typename = int> class image {
 public:
 typedef type_from_x_iterator<iterator_type_from_pixel::type>::view_t view_t;
 image(int);
};
template <typename Pixel, bool IsPlanar, typename Alloc>
  typename image<Pixel, 0>::view_t view(image<Pixel, IsPlanar, Alloc>);
template <typename Op> void measure_time(Op p1) {
  for (;;)
    p1();
}
template <typename, typename> struct fill_nongil_t;
template <typename T, typename P>
  struct fill_nongil_t<
      image_view<memory_based_2d_locator<
  memory_based_step_iterator<pixel<T, layout<vector3<int>>> *>>>,
  P> {
    typedef image_view<memory_based_2d_locator<
      memory_based_step_iterator<pixel<T, layout<vector3<int>>> *>>> View;
    View _v;
    P _p;
 fill_nongil_t(View p1, P) : _v(p1) {}
    void operator()() {
      T *first = (T *)_v.row_begin(0);
      T last;
      while (first != &last) {
	first[0] = at_c<0>(_p);
	first[1] = at_c<1>(_p);
	first[2] = at_c<2>(_p);
	first += 3;
      }
    }
};
template <typename, typename> void test_fill(int) {
  image<int, 0>::view_t __trans_tmp_1;
  image<int, 0> im(0);
  __trans_tmp_1 = view(im);
  measure_time(fill_nongil_t<
	             image_view<memory_based_2d_locator<memory_based_step_iterator<
	       pixel<unsigned char, layout<vector3<int>>> *>>>,
	       pixel<unsigned, int>>(__trans_tmp_1, pixel<unsigned, int>()));
}
void performance_testtest_method() {
  test_fill<image_view<int>, pixel<unsigned, int>>(0);
}
