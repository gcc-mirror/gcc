// { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } }
// { dg-options "-O3 -fselective-scheduling2" }

namespace std {

typedef long unsigned int size_t;

template<typename _Tp> class new_allocator { public: typedef size_t size_type; typedef _Tp* pointer; };
template<typename _Tp> class allocator: public new_allocator<_Tp> { public: typedef size_t size_type; template<typename _Tp1> struct rebind { typedef allocator<_Tp1> other; }; };

class back_insert_iterator { };
template<typename _Container> back_insert_iterator back_inserter(_Container& __x) { };

class vector { };

struct _List_node_base { };
struct _List_node : public _List_node_base { };
template<typename _Tp> struct _List_iterator { typedef _List_iterator<_Tp> _Self; typedef _Tp& reference; explicit _List_iterator(_List_node_base* __x) : _M_node(__x) { } reference operator*() const { } _Self& operator++() { } bool operator!=(const _Self& __x) const { return _M_node != __x._M_node; } _List_node_base* _M_node; };
template<typename _Tp, typename _Alloc> class _List_base { protected: typedef typename _Alloc::template rebind<_List_node >::other _Node_alloc_type; struct _List_impl : public _Node_alloc_type { _List_node_base _M_node; }; _List_impl _M_impl; };
template<typename _Tp, typename _Alloc = std::allocator<_Tp> > class list : protected _List_base<_Tp, _Alloc> { public: typedef _Tp value_type; typedef _List_iterator<_Tp> iterator; iterator begin() { } iterator end() { return iterator(&this->_M_impl._M_node); } };

namespace tr1 { template<typename _Tp, size_t _Nm> struct array { typedef _Tp value_type; typedef const value_type& const_reference; typedef const value_type* const_iterator; typedef size_t size_type; value_type _M_instance[_Nm ? _Nm : 1]; const_iterator begin() const { return const_iterator(&_M_instance[0]); } const_reference operator[](size_type __n) const { return _M_instance[__n]; } }; }
}

namespace X {

class Object { };
struct Has_qrt { };
template <typename F> struct qrt_or_not { typedef const typename F::result_type & type; };
template <typename Functor, typename P1 = void> struct Qualified_result_of : qrt_or_not<Functor> { };

using std::tr1::array;

template <class R_> class Point_2 : public R_::Kernel_base::Point_2 {
public:
  typedef typename R_::Kernel_base::Point_2 RPoint_2;
  typedef RPoint_2 Rep;
  const Rep& rep() const { }
};

template <class R_> class Vector_2 : public R_::Kernel_base::Vector_2 {
public:
  typedef typename R_::Kernel_base::Vector_2 RVector_2;
  typedef RVector_2 Rep;
  const Rep& rep() const { return *this; }
  typedef R_ R;
  typename Qualified_result_of<typename R::Compute_x_2,Vector_2>::type x() const { return R().compute_x_2_object()(*this); }
  typename Qualified_result_of<typename R::Compute_y_2,Vector_2>::type y() const { return R().compute_y_2_object()(*this); }
  typename Qualified_result_of<typename R::Compute_y_2,Vector_2>::type cartesian(int i) const { return (i==0) ? x() : y(); }
  typename Qualified_result_of<typename R::Compute_hx_2,Vector_2>::type hx() const { return R().compute_hx_2_object()(*this); }
  typename Qualified_result_of<typename R::Compute_hy_2,Vector_2>::type hy() const { return R().compute_hy_2_object()(*this); }
  typename Qualified_result_of<typename R::Compute_hw_2,Vector_2>::type hw() const { return R().compute_hw_2_object()(*this); }
  typename Qualified_result_of<typename R::Compute_hx_2,Vector_2>::type homogeneous(int i) const { return (i==0) ? hx() : (i==1)? hy() : hw(); }
};

template <class R_> class Segment_2 : public R_::Kernel_base::Segment_2 { };
template <class R_> class Iso_rectangle_2 : public R_::Kernel_base::Iso_rectangle_2 { };

template <typename T, int i > const T& constant() { static const T t(i); return t; }
template <class T, class Alloc = std::allocator<T > > class Handle_for { struct RefCounted { T t; }; typedef typename Alloc::template rebind<RefCounted>::other Allocator; typedef typename Allocator::pointer pointer; pointer ptr_; public: typedef T element_type; const T * Ptr() const { return &(ptr_->t); } };
template <class T, class Allocator> const T& get(const Handle_for<T, Allocator> &h) { return *(h.Ptr()); }

template <class R_> class PointC2 {
public:
  typedef typename R_::Vector_2 Vector_2; Vector_2 base;
  typedef typename Vector_2::Cartesian_const_iterator Cartesian_const_iterator; Cartesian_const_iterator cartesian_begin() const { return base.cartesian_begin(); }
};

template <class R_> class VectorC2 {
public:
  typedef typename R_::FT FT;
  typedef array<FT, 2> Rep;
  typedef typename R_::template Handle<Rep>::type Base;
  Base base;
  typedef typename Rep::const_iterator Cartesian_const_iterator;
  const FT & x() const { return X::get(base)[0]; }
  const FT & y() const { return X::get(base)[1]; }
  const FT & hx() const { return x(); }
  const FT & hy() const { return y(); }
  const FT & hw() const { return constant<FT, 1>(); }
  Cartesian_const_iterator cartesian_begin() const { return X::get(base).begin(); }
};

template <class R_> class SegmentC2 { };
template <class R_> class Iso_rectangleC2 { };

namespace internal {
  template <class K> class Segment_2_Iso_rectangle_2_pair {
    public:
      enum Intersection_results { NO_INTERSECTION };
      Segment_2_Iso_rectangle_2_pair(typename K::Segment_2 const *seg, typename K::Iso_rectangle_2 const *rect) ;
      Intersection_results intersection_type() const;
      mutable Intersection_results _result;
      typename K::Point_2 _ref_point;
      typename K::Vector_2 _dir;
      typename K::Point_2 _isomin;
      typename K::Point_2 _isomax;
      mutable typename K::FT _min, _max;
  };
  template <class K> Object intersection( const typename K::Segment_2 &seg, const typename K::Iso_rectangle_2 &iso, const K&) {
    typedef Segment_2_Iso_rectangle_2_pair<K> is_t; is_t ispair(&seg, &iso); switch (ispair.intersection_type()) { }
  }
  template <class K> typename Segment_2_Iso_rectangle_2_pair<K>::Intersection_results Segment_2_Iso_rectangle_2_pair<K>::intersection_type() const {
    typedef typename K::RT RT;
    typedef typename K::FT FT;
    typename K::Construct_cartesian_const_iterator_2 construct_cccit;
    typename K::Cartesian_const_iterator_2 ref_point_it = construct_cccit(_ref_point);
    typename K::Cartesian_const_iterator_2 end = construct_cccit(_ref_point, 0);
    typename K::Cartesian_const_iterator_2 isomin_it = construct_cccit(_isomin);
    typename K::Cartesian_const_iterator_2 isomax_it = construct_cccit(_isomax);
    for (unsigned int i=0; ref_point_it != end; ++i, ++ref_point_it, ++isomin_it, ++isomax_it) {
      if (_dir.homogeneous(i) == RT(0)) {
        if ( *(ref_point_it) <*(isomin_it) ) {
          _result = NO_INTERSECTION;
        }
        if ( *(ref_point_it) > *(isomax_it)) {
          _result = NO_INTERSECTION;
        }
      } else {
        FT newmin, newmax;
        if (_dir.homogeneous(i) > RT(0)) {
          newmin = ( *(isomin_it) - (*ref_point_it)) / _dir.cartesian(i);
          newmax = ( *(isomax_it) - (*ref_point_it)) / _dir.cartesian(i);
        } else {
          newmin = ( (*isomax_it) - (*ref_point_it)) / _dir.cartesian(i);
          newmax = ( (*isomin_it) - (*ref_point_it)) / _dir.cartesian(i);
        }
        if (newmin > _min) _min = newmin;
        if (newmax <_max) _max = newmax;
        if (_max <_min) { return _result; }
      }
    }
  }
}

template <class K> Object intersection(const Segment_2<K> &seg, const Iso_rectangle_2<K> &iso) { typedef typename K::Intersect_2 Intersect; return Intersect()(seg, iso); }

namespace CommonKernelFunctors {
  template <typename K> class Construct_cartesian_const_iterator_2 {
    typedef typename K::Point_2 Point_2;
    typedef typename K::Cartesian_const_iterator_2 Cartesian_const_iterator_2;
public:
    typedef Cartesian_const_iterator_2 result_type;
    Cartesian_const_iterator_2 operator()( const Point_2& p) const { return p.rep().cartesian_begin(); }
    Cartesian_const_iterator_2 operator()( const Point_2& p, int) const { }
  };
  template <typename K> class Intersect_2 {
    typedef typename K::Object_2 Object_2;
  public:
    typedef Object_2 result_type;
    template <class T1, class T2> Object_2 operator()(const T1& t1, const T2& t2) const { return internal::intersection(t1, t2, K()); }
  };
}

namespace CartesianKernelFunctors {
  using namespace CommonKernelFunctors;
  template <typename K> class Compute_x_2 : Has_qrt {
    typedef typename K::FT FT;
    typedef typename K::Vector_2 Vector_2;
  public:
    typedef FT result_type;
    const result_type & operator()(const Vector_2& v) const { return v.rep().x(); }
  };
  template <typename K> class Compute_y_2 : Has_qrt {
    typedef typename K::FT FT;
    typedef typename K::Vector_2 Vector_2;
  public:
    typedef FT result_type;
    const result_type & operator()(const Vector_2& v) const { return v.rep().y(); }
  };
  template <typename K> class Compute_hx_2 : public Has_qrt {
    typedef typename K::FT FT;
    typedef typename K::Vector_2 Vector_2;
  public:
    typedef FT result_type;
    const result_type & operator()(const Vector_2& v) const { return v.rep().hx(); }
  };
  template <typename K> class Compute_hy_2 : public Has_qrt {
    typedef typename K::FT FT;
    typedef typename K::Vector_2 Vector_2;
  public:
    typedef FT result_type;
    const result_type & operator()(const Vector_2& v) const { return v.rep().hy(); }
  };
  template <typename K> class Compute_hw_2 : public Has_qrt {
    typedef typename K::FT FT;
    typedef typename K::Vector_2 Vector_2;
  public:
    typedef FT result_type;
    const result_type & operator()(const Vector_2& v) const { return v.rep().hw(); }
  };
}

template <typename K_, typename FT_> struct Cartesian_base {
  typedef K_ Kernel;
  typedef X::Object Object_2;
  typedef PointC2<Kernel> Point_2;
  typedef VectorC2<Kernel> Vector_2;
  typedef SegmentC2<Kernel> Segment_2;
  typedef Iso_rectangleC2<Kernel> Iso_rectangle_2;
  typedef typename array<FT_, 2>::const_iterator Cartesian_const_iterator_2;
};

template <typename K_base, typename Kernel_ > struct Type_equality_wrapper : public K_base {
  typedef K_base Kernel_base;
  typedef X::Point_2<Kernel_> Point_2;
  typedef X::Vector_2<Kernel_> Vector_2;
  typedef X::Segment_2<Kernel_> Segment_2;
  typedef X::Iso_rectangle_2<Kernel_> Iso_rectangle_2;
};

template <typename FT_, typename Kernel_ > struct Cartesian_base_ref_count : public Cartesian_base<Kernel_, FT_ > {
  typedef FT_ RT;
  typedef FT_ FT;
  template <typename T > struct Handle { typedef Handle_for<T> type; };
  typedef Kernel_ K;
  typedef CartesianKernelFunctors::Compute_x_2<K> Compute_x_2;
  Compute_x_2 compute_x_2_object() const { }
  typedef CartesianKernelFunctors::Compute_y_2<K> Compute_y_2;
  Compute_y_2 compute_y_2_object() const { }
  typedef CartesianKernelFunctors::Compute_hx_2<K> Compute_hx_2;
  Compute_hx_2 compute_hx_2_object() const { }
  typedef CartesianKernelFunctors::Compute_hy_2<K> Compute_hy_2;
  Compute_hy_2 compute_hy_2_object() const { }
  typedef CartesianKernelFunctors::Compute_hw_2<K> Compute_hw_2;
  Compute_hw_2 compute_hw_2_object() const { }
  typedef CartesianKernelFunctors::Construct_cartesian_const_iterator_2<K> Construct_cartesian_const_iterator_2;
  typedef CartesianKernelFunctors::Intersect_2<K> Intersect_2;
};

template <typename FT_ > struct Cartesian : public Type_equality_wrapper<Cartesian_base_ref_count<FT_, Cartesian<FT_> >, Cartesian<FT_> > { };

template <class Kernel> class Ipelet_base {
public:
  typedef typename X::Point_2<Kernel> Point_2;
  typedef typename Kernel::Segment_2 Segment_2;
  typedef typename Kernel::Iso_rectangle_2 Iso_rectangle_2;

  Iso_rectangle_2 read_active_objects () const { }
  struct Voronoi_from_tri{ std::list<Segment_2> seg_list; };

  template <class T,class output_iterator> bool cast_into_seg(const T& obj,const Iso_rectangle_2& bbox,output_iterator out_it) const{ X::intersection(obj,bbox); }
  template<class iterator,class output_iterator> void cast_into_seg(const iterator first,const iterator end, const Iso_rectangle_2& bbox, output_iterator out_it) const { for (iterator it=first; it!=end; ++it) cast_into_seg(*it,bbox,out_it); }
  void draw_dual_(Voronoi_from_tri& v_recup,const Iso_rectangle_2& bbox) const { std::vector seg_cont; cast_into_seg(v_recup.seg_list.begin(),v_recup.seg_list.end(),bbox,std::back_inserter(seg_cont)); }
  void draw_dual_in_ipe(const Iso_rectangle_2& bbox) const { Voronoi_from_tri v_recup; draw_dual_(v_recup,bbox); }
};

typedef X::Cartesian<double> Kernel;

class diagrammeIpelet : public X::Ipelet_base<Kernel> { void protected_run(); };
void diagrammeIpelet::protected_run() { Iso_rectangle_2 bbox = read_active_objects( ); draw_dual_in_ipe(bbox); }

}
