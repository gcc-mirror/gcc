// { dg-do compile { target *-*-linux* } }
// { dg-options "-O3 -std=c++20 -ftrivial-auto-var-init=zero -march=x86-64-v3 -fPIC -w -mtls-dialect=gnu " }

template <int> using b = int;
template <typename, typename> struct e;
struct m {};
template <class> struct aa;
template <typename> struct j;
using h = aa<j<int>>;
template <typename> using a = h;
template <typename> using k = h;
template <typename d> struct p {
  d ad;
};
struct ac {};
template <typename> struct al;
template <typename g> struct al<g *> {
  typedef g &ah;
};
template <typename z> z ::i q(z);
struct w {
  e<m, m> *l;
  al<e<m, m> *>::ah operator*() { return *l; }
  bool operator==(w) { return l; }
};
template <typename g, int o> struct n {
  using af = g[o];
};
template <typename g, int o> struct ak {
  n<g, o>::af am;
};
void ab();
template <typename ai> void ar(ai r, ac) {
  ai ag;
  for (; r != ag; ++r)
    ab(), *r;
}
template <typename ai> void as(ai r, ac) { ar(r, q(r)); }
struct {
  template <typename ai> void aj(ai r, ai) { as(r, q(r)); }
} ao;
namespace ap {
template <int at> struct bb {
  static const bool ax = at;
};
} // namespace ap
using ap::bb;
namespace av {
template <class aq, aq> struct c;
template <bool au> struct c<bool, au> {
  static const bool ax = au;
  operator bb<au>() {
    void *bc;
    return *reinterpret_cast<bb<au> *>(bc);
  }
};
template <class, class> struct ae : c<bool, false> {};
template <class aq> struct ae<aq, aq> : c<bool, true> {};
namespace aw {
using namespace ap;
}
} // namespace av
namespace ap {
template <int> struct bd {
  typedef bd<1> bh;
};
} // namespace ap
namespace av {
namespace aw {
template <typename, typename = int> struct be;
template <int, typename, typename, typename, typename> struct bn : bb<true> {};
template <typename bg, typename bj, typename ay, typename az>
struct bn<false, bg, bj, ay, az> : bn<bg ::ax, bj, ay, az, bb<false>> {};
template <>
struct bn<false, bb<false>, bb<false>, bb<false>, bb<false>> : bb<false> {};
template <typename bg, typename bj = int, typename ay = bb<false>,
          typename az = bb<false>, typename bl = bb<false>>
struct br : bn<bg ::ax, bj, ay, az, bl> {};
} // namespace aw
template <class aq> struct bi {
  typedef aq h;
};
template <typename aq> bi<aq>::h bf();
template <class> struct ba : c<bool, false> {};
template <class aq> struct ba<aq const> : c<bool, true> {};
template <class aq> struct bk {
  typedef aq const h;
};
template <class aq> struct bq {
  typedef aq h;
};
template <class aq> struct bu {
  typedef aq::h h;
};
template <class bo> struct by {
  typedef bo ::ah h;
};
template <class aq> struct bm {
  typedef aq h;
};
template <class aq> struct bm<aq const> {
  typedef aq h;
};
namespace aw {
template <int, typename bg, typename> struct bp {
  typedef bg h;
};
template <typename bg, typename bj> struct bp<false, bg, bj> {
  typedef bj h;
};
template <typename bg, typename bj, typename ay = int> struct cg {
  typedef bp<bg::ax, bj, ay>::h h;
};
template <typename C, typename bt, typename cd = int> struct bs {
  typedef cg<C, bt, cd>::h ::h h;
};
template <int, typename, typename, typename, typename> struct bx;
template <typename bg, typename bj, typename ay, typename az>
struct bx<true, bg, bj, ay, az> : bx<bg ::ax, bj, ay, az, bb<true>> {};
template <>
struct bx<true, bb<true>, bb<true>, bb<true>, bb<true>> : bb<true> {};
template <typename bg, typename bj>
struct bw : bx<bg ::ax, bj, bb<true>, bb<true>, bb<true>> {};
} // namespace aw
template <typename bv, typename bz> struct cb {
  template <typename cc> static void ca(cc);
  template <typename ch, typename cc> static decltype(ca<cc>(bf<ch>())) cr(int);
  template <typename, typename> static int cr(...);
  static const bool ax = sizeof(cr<bv, bz>(0)) == 1;
};
template <typename bv, typename bz> struct cm {
  static const bool ax = cb<bv, bz>::ax;
};
template <class bv, class bz> struct cl {
  typedef cm<bv, bz> h;
};
template <typename bv, typename bz> struct ci {
  typedef cl<bv, bz>::h h;
};
template <typename bv, typename bz> struct co : ci<bv, bz>::h {};
template <class bv, class bz> struct ce : c<bool, co<bv, bz>::ax> {};
namespace aw {
template <typename aq> struct cf {
  typedef aq h;
};
} // namespace aw
template <int> struct arg {
  template <typename ck, typename> struct apply {
    typedef ck h;
  };
};
template <> struct arg<2> {
  template <typename, typename cu> struct apply {
    typedef cu h;
  };
};
struct cj {};
template <class cq> struct cx : aw::bs<ce<cq, cj>, aw::cf<cq>> {};
template <class bo> struct cw : cx<typename bo::i> {};
template <class ct, class cp, class dc>
struct cn : aw::bs<aw::bw<ce<ct, cj>, ce<dc, cp>>, aw::cf<ac>> {};
template <class aq> struct dd : aw::br<ce<aq, ac>, ce<aq, int>> {};
template <class cs, class ct> struct cz : cs, ct {};
template <class cp, class dc> struct da {
  typedef cn<cj, cp, dc>::h category;
  typedef aw::cg<ae<cj, int>, category, cz<category, cj>>::h h;
};
template <class cy, class cp, class dc>
struct s : aw::bs<dd<cy>, aw::cf<cy>, da<cp, dc>> {};
namespace aw {
template <typename df, typename bg> struct de : df::apply<bg> {};
template <typename df, typename bg, typename bj>
struct u : df::apply<bg, bj> {};
template <typename df, typename bg, typename bj>
struct db : df::apply<bg, bj> {};
template <typename aq> struct bh {
  typedef aq::bh h;
};
template <typename, typename, typename, typename> struct cv;
template <int dk, typename ck, typename cu, typename dm>
struct cv<arg<dk>, ck, cu, dm> {
  typedef db<arg<dk>, ck, cu>::h h;
};
template <typename df, typename bg> struct dh {
  template <typename ck, typename cu = int> struct apply {
    typedef de<df, typename cv<bg, ck, cu, int>::h>::h h;
  };
};
template <typename df, typename bg, typename ck, typename cu, typename dm>
struct cv<dh<df, bg>, ck, cu, dm> {
  typedef db<dh<df, bg>, ck, cu>::h h;
};
template <typename df, typename bg, typename bj> struct dg {
  template <typename ck, typename cu> struct apply {
    typedef u<df, typename cv<bg, ck, cu, int>::h,
              typename cv<bj, ck, cu, int>::h>::h h;
  };
};
template <typename aq> struct di {
  typedef aq::h h;
};
template <template <typename> class df> struct dl {
  template <typename ck> struct apply : di<df<ck>> {};
};
template <int dk, typename dn> struct be<arg<dk>, dn> {
  typedef bb<true> dj;
  typedef arg<dk> result_;
};
template <typename, template <typename> class, typename> struct x;
template <template <typename> class df, typename L1>
struct x<bb<true>, df, L1> {
  typedef dh<dl<df>, typename L1::result_> h;
};
template <template <typename> class df, typename bg, typename dn>
struct be<df<bg>, dn> {
  typedef be<bg> l1;
  typedef x<typename l1::dj, df, l1>::h h;
};
template <typename df, typename bg, typename bj>
struct apply2 : u<df, bg, bj> {};
} // namespace aw
namespace iterators {
struct always_bool2 {
  template <class, class> struct apply {
    typedef bool h;
  };
};
template <class cp, class cy, class dc> struct y {
  typedef s<cy, cp, dc>::h i;
};
template <class, class, class, class, class, bool> class iterator_facade_base;
struct iterator_core_access {
  template <class Facade> static Facade::ah dereference(Facade &r) {
    return r.dereference();
  }
  template <class Facade> static void increment(Facade &r) { r.increment(); }
  template <class Facade1, class Facade2>
  static bool equal(Facade1 &r, Facade2 p2) {
    return r.equal(p2);
  }
};
template <class Derived, class Value, class cy, class dc, class Difference>
struct iterator_facade_base<Derived, Value, cy, dc, Difference, false> {
  typedef dc ah;
  typedef y<Value, cy, dc>::i i;
  ah operator*() { return iterator_core_access::dereference(derived()); }
  void operator++() { iterator_core_access::increment(derived()); }
  Derived &derived() { return *static_cast<Derived *>(this); }
};
template <class Derived, class Value, class cy, class dc, class = long>
struct iterator_facade
    : iterator_facade_base<Derived, Value, cy, dc, int, ce<cj, int>::ax> {};
template <class Derived1, class V1, class TC1, class Reference1,
          class Difference1, class Derived2, class V2, class TC2,
          class Reference2, class Difference2>
bq<typename aw::apply2<always_bool2, Derived1, Derived2>::h>::h
operator==(iterator_facade<Derived1, V1, TC1, Reference1, Difference1> const &r,
           iterator_facade<Derived2, V2, TC2, Reference2, Difference2> p2) {
  return iterator_core_access::equal(*static_cast<Derived1 const *>(&r),
                                     *static_cast<Derived2 *>(&p2));
}
} // namespace iterators
using iterators::iterator_facade;
template <class aq, class DefaultNullaryFn>
struct ia_dflt_help : aw::bs<ae<aq, int>, DefaultNullaryFn, aw::cf<aq>> {};
template <class Derived, class Base, class dc> struct iterator_adaptor_base {
  typedef iterator_facade<Derived, int, typename ia_dflt_help<int, cw<Base>>::h,
                          typename ia_dflt_help<dc, int>::h>
      h;
};
template <class Derived, class Base, class, class, class dc, class = int>
struct iterator_adaptor : iterator_adaptor_base<Derived, Base, dc>::h {
  Base an() const { return m_iterator; }
  template <class OtherDerived, class OtherIterator, class V, class C, class R,
            class D>
  bool equal(iterator_adaptor<OtherDerived, OtherIterator, V, C, R, D>) const {
    return m_iterator == an();
  }
  void increment() { ++m_iterator; }
  Base m_iterator;
};
template <typename> struct result_of;
template <typename df, typename... Args> struct result_of<df(Args...)> {
  typedef decltype(df()(bf<Args>()...)) h;
};
namespace iterators {
template <class> class transform_iterator;
template <class UnaryFunc, class bo> struct transform_iterator_base {
  typedef iterator_adaptor<
      transform_iterator<UnaryFunc>, bo, UnaryFunc, int,
      typename ia_dflt_help<int, result_of<UnaryFunc(typename bo::ah)>>::h>
      h;
};
template <typename> class zip_iterator;
template <class UnaryFunc>
struct transform_iterator
    : transform_iterator_base<UnaryFunc, zip_iterator<int>>::h {
  void dereference() { m_f(*this->an()); }
  UnaryFunc m_f;
};
template <class UnaryFunc>
transform_iterator<UnaryFunc> make_transform_iterator(zip_iterator<int>,
                                                      UnaryFunc);
} // namespace iterators
namespace tuples {
template <class, class> struct cons;
template <class = int, class = int, class = int, class = int, class = int,
          class = int, class = int, class = int, class = int, class = int>
class tuple;
template <class aq> struct access_traits {
  typedef const aq &const_type;
  typedef aq &non_const_type;
};
template <long, class HT, class TT>
access_traits<HT>::const_type get(cons<HT, TT>);
template <class aq> struct wrap_non_storeable_type {
  typedef aq h;
};
template <class HT, class TT> struct cons {
  typedef HT head_type;
  typedef TT tail_type;
  typedef wrap_non_storeable_type<head_type>::h stored_head_type;
  stored_head_type head;
  tail_type tail;
  access_traits<stored_head_type>::non_const_type get_head() { return head; }
  access_traits<tail_type>::non_const_type get_tail() { return tail; }
  access_traits<stored_head_type>::const_type get_head() const { return head; }
  access_traits<tail_type>::const_type get_tail() const { return tail; }
};
template <class T0, class bg, class, class, class, class, class, class, class,
          class>
struct map_tuple_to_cons {
  typedef cons<T0, typename map_tuple_to_cons<bg, int, int, int, int, int, int,
                                              int, int, int>::h>
      h;
};
template <>
struct map_tuple_to_cons<int, int, int, int, int, int, int, int, int, int> {
  typedef int h;
};
template <class T0, class bg, class, class, class, class, class, class, class,
          class>
struct tuple
    : map_tuple_to_cons<T0, bg, int, int, int, int, int, int, int, int>::h {};
} // namespace tuples
using tuples::tuple;
} // namespace av
template <class R_> struct aa {
  ak<typename R_::FT, 3> an;
};
template <typename K> struct Compare_xy_2 {
  typedef K::Point_2 Point_2;
  void operator()(Point_2, Point_2);
};
template <typename K_, typename FT_> struct Cartesian_base {
  typedef FT_ FT;
  typedef m Point_2;
  typedef aa<K_> Line_2;
};
template <typename K_base> struct Type_equality_wrapper : K_base {};
template <typename FT_, typename Kernel_>
struct Cartesian_base_no_ref_count : Cartesian_base<Kernel_, FT_> {};
template <typename FT_>
struct j : Type_equality_wrapper<Cartesian_base_no_ref_count<FT_, j<FT_>>> {};
template <class C2E> struct Filtered_predicate {
  C2E c2e;
  Compare_xy_2<j<double>> ep;
  template <typename... Args> void operator()(Args... r) { ep(c2e(r)...); }
};
struct Mpzf {
  ~Mpzf();
};
struct Cartesian_converter {
  j<Mpzf>::Line_2 operator()(Type_equality_wrapper<j<double>>::Line_2);
};
namespace av {
namespace aw {
template <typename> struct begin_impl;
template <typename> struct end_impl;
template <typename> struct sequence_tag;
template <typename Sequence> struct begin {
  typedef sequence_tag<Sequence>::h tag_;
  typedef begin_impl<tag_>::template apply<Sequence>::h h;
};
template <typename Sequence> struct ab {
  typedef sequence_tag<Sequence>::h tag_;
  typedef end_impl<tag_>::template apply<Sequence>::h h;
};
const long ax = -1;
template <int, typename, typename, typename, typename> struct fold_impl;
template <typename First, typename Last, typename State, typename ForwardOp>
struct fold_impl<-1, First, Last, State, ForwardOp>
    : fold_impl<-1, typename bh<First>::h, Last,
                typename apply2<ForwardOp, State, typename First::h>::h,
                ForwardOp> {};
template <typename Last, typename State, typename ForwardOp>
struct fold_impl<-1, Last, Last, State, ForwardOp> {
  typedef State state;
};
template <typename Sequence, typename State, typename ForwardOp> struct fold {
  typedef fold_impl<ax, typename begin<Sequence>::h, typename ab<Sequence>::h,
                    State, ForwardOp>::state h;
};
template <typename> struct push_back_impl;
template <typename Sequence = int, typename aq = int>
struct push_back
    : push_back_impl<typename sequence_tag<Sequence>::h>::apply<Sequence, aq> {
};
struct has_push_back {
  static const bool ax = sizeof(0);
};
template <> struct push_back<> {
  template <typename bg, typename bj> struct apply : push_back<bg, bj> {};
};
template <typename Sequence, typename Operation> struct inserter {
  typedef Sequence state;
  typedef Operation operation;
};
template <typename Sequence>
struct back_inserter : inserter<Sequence, push_back<>> {};
template <typename> struct clear_impl;
template <typename Sequence>
struct clear : clear_impl<typename sequence_tag<Sequence>::h>::apply<Sequence> {
};
template <typename Seq, typename Op, typename In>
struct transform1_impl
    : fold<Seq, typename In::state,
           dg<typename In::operation, arg<1>, dh<typename be<Op>::h, arg<2>>>> {
};
template <typename P1>
struct transform1
    : cg<has_push_back,
         transform1_impl<P1, by<arg<1>>,
                         back_inserter<typename clear<P1>::h>>>::h {};
template <typename Seq1> struct transform {
  typedef bs<br<bb<true>>, transform1<Seq1>>::h h;
};
} // namespace aw
} // namespace av
template <typename, typename, typename> struct Type_mapper_impl;
template <typename K1, typename K2>
struct Type_mapper_impl<typename K1::Line_2, K1, K2> {
  typedef K2::Line_2 h;
};
auto call_once___callable = [] {};
struct once_flag {
  struct _Prepare_execution;
} __thread *__once_callable;
struct once_flag::_Prepare_execution {
  template <typename _Callable> _Prepare_execution(_Callable) {
    __once_callable = nullptr;
  }
};
template <typename _Callable> void call_once(_Callable) {
  once_flag::_Prepare_execution __exec(call_once___callable);
}
template <typename ET> auto approx(e<aa<j<int>>, ET> r) { return r.approx(); }
template <typename ET> ET exact(e<m, ET> r) { return r.exact(); }
once_flag once;
template <typename AT_, typename ET> struct Lazy_rep {
  AT_ approx();
  ET exact_unsafe();
  ET exact() {
    call_once(once);
    return exact_unsafe();
  }
};
struct Exact_converter {
  template <typename aq> auto operator()(aq r) { return exact(r); }
};
template <typename AT_, typename ET_> struct e {
  typedef Lazy_rep<AT_, ET_> Self_rep;
  e(Self_rep *);
  auto approx() { return ptr()->approx(); }
  ET_ exact() { return ptr()->exact(); }
  Self_rep *ptr();
};
template <typename LK> struct Lazy_construction {
  auto operator()() {
    return typename Type_mapper_impl<a<k<int>>, typename LK::Approximate_kernel,
                                     LK>::h(0);
  }
};
template <typename, typename>
using Epick_with_filtered_predicates = Type_equality_wrapper<j<double>>;
struct Epic_converter {
  p<Epick_with_filtered_predicates<double, int>::Line_2>
  operator()(j<int>::Line_2);
};
template <typename FP, typename EpicP> struct Static_filtered_predicate {
  FP fp;
  EpicP epicp;
  template <typename A1> void operator()(A1 r) {
    Epic_converter convert;
    auto __trans_tmp_4 = approx(r), aa1 = convert(__trans_tmp_4);
    epicp(aa1.ad);
  }
  template <typename A1, typename A2> void operator()(A1 r, A2 p2) {
    fp(r, p2);
  }
};
Static_filtered_predicate<Filtered_predicate<Exact_converter>, int>
compare_xy_2_object();
Static_filtered_predicate<int, Cartesian_converter> is_vertical_2_object();
struct Lazy_kernel_base {
  typedef j<int> Approximate_kernel;
  typedef e<Approximate_kernel::Line_2, int> Line_2;
};
template <typename, typename> using Epeck_lazy_base = Lazy_kernel_base;
template <typename FT, typename>
using Epeck_lazy_base_with_type_equality =
    Type_equality_wrapper<Epeck_lazy_base<FT, class Epeck>>;
struct Epeck : Epeck_lazy_base_with_type_equality<double, Epeck> {};
struct Construct_x_monotone_curve_2 {
  void operator()(e<m, m> r, e<m, m> p2) {
    Lazy_construction<Epeck> __trans_tmp_9;
    auto line = __trans_tmp_9();
    compare_xy_2_object()(r, p2);
    is_vertical_2_object()(line);
  }
};
Construct_x_monotone_curve_2 construct_curve_2_object();
namespace av {
w last1;
typedef e<m, m> *iterator2_t;
struct join_iterator_union {
  iterator2_t &it2() { return m_it2; }
  e<m, m> &dereference(unsigned r) {
    if (r)
      return *m_it2;
    return *m_it1;
  }
  w m_it1;
  iterator2_t m_it2;
};
struct join_iterator : iterator_facade<join_iterator, int, int, e<m, m> &> {
  w __trans_tmp_24;
  void increment() {
    if (m_section)
      ;
    else if (__trans_tmp_24 == last1)
      m_it.it2() = 0;
  }
  ah dereference() { return m_it.dereference(m_section); }
  bool equal(join_iterator) const { return m_section; }
  int m_section;
  join_iterator_union m_it;
};
} // namespace av
template <typename InputIterator> void construct_polycurve(InputIterator r) {
  InputIterator begin;
  ao.aj(begin, r);
}
struct Polycurve_2 {
  template <typename InputIterator>
  Polycurve_2(InputIterator, InputIterator p2) {
    construct_polycurve(p2);
  }
};
struct Arr_polycurve_traits_2 {
  struct Construct_curve_2 {
    template <typename ForwardIterator>
    void operator()(ForwardIterator r, ForwardIterator p2) {
      Polycurve_2(r, p2);
    }
  };
};
namespace av {
namespace fusion {
namespace traits {
template <typename> struct tag_of;
template <class T0, class bg, class bj, class ay, class az, class bl, class T6,
          class T7, class T8, class T9>
struct tag_of<tuple<T0, bg, bj, ay, az, bl, T6, T7, T8, T9>> {
  typedef int h;
};
template <class Head, class Tail> struct tag_of<tuples::cons<Head, Tail>> {
  typedef int h;
};
} // namespace traits
} // namespace fusion
namespace aw {
template <class T0, class bg, class bj, class ay, class az, class bl, class T6,
          class T7, class T8, class T9>
struct sequence_tag<tuple<T0, bg, bj, ay, az, bl, T6, T7, T8, T9>> {
  typedef int h;
};
template <class Head, class Tail>
struct sequence_tag<tuples::cons<Head, Tail>> {
  typedef int h;
};
} // namespace aw
namespace fusion {
namespace detail {
template <typename Sequence> struct tag_of_impl {
  typedef Sequence::fusion_tag h;
};
} // namespace detail
namespace traits {
template <typename Sequence> struct tag_of : detail::tag_of_impl<Sequence> {};
} // namespace traits
namespace detail {
template <typename aq> struct tag_of : traits::tag_of<typename bm<aq>::h> {};
} // namespace detail
struct iterator_facade_tag;
template <typename> struct next_impl {
  template <typename bo> struct apply : bo::bh<bo> {};
};
namespace result_of {
template <typename bo>
struct bh : next_impl<typename bo::fusion_tag>::apply<bo> {};
} // namespace result_of
template <typename bo> result_of::bh<bo>::h bh(bo r) {
  return result_of::bh<bo>::call(r);
}
template <typename> struct equal_to_impl {
  template <typename I1, typename I2>
  struct apply : ae<typename bk<I1>::h, typename bk<I2>::h> {};
};
template <> struct equal_to_impl<iterator_facade_tag> {
  template <typename, typename, typename, typename> struct dispatch;
  template <typename It1, typename It2, typename dn>
  struct dispatch<It1, It2, dn, dn> : It1::equal_to<It1, It2> {};
  template <typename It1, typename It2>
  struct apply
      : dispatch<It1, It2, typename It1::fusion_tag, typename It2::fusion_tag> {
  };
};
namespace result_of {
template <typename I1, typename I2>
struct equal_to : equal_to_impl<typename I1::fusion_tag>::apply<I1, I2> {};
} // namespace result_of
struct iterator_facade {
  typedef iterator_facade_tag fusion_tag;
};
template <typename Cons = int> struct boost_tuple_iterator : iterator_facade {
  typedef Cons cons_type;
  boost_tuple_iterator(Cons &r) : cons(r) {}
  Cons &cons;
  template <typename>
  struct value_of : aw::cf<typename cons_type::head_type> {};
  template <typename bo> struct deref {
    typedef value_of<bo>::h element;
    typedef aw::cg<
        ba<cons_type>, typename tuples::access_traits<element>::const_type,
        typename tuples::access_traits<element>::non_const_type>::h h;
    static h call(bo r) { return r.cons.get_head(); }
  };
  template <typename bo> struct bh {
    typedef cons_type::tail_type tail_type;
    typedef boost_tuple_iterator<
        typename aw::bs<ba<cons_type>, bk<tail_type>, aw::cf<tail_type>>::h>
        h;
    static h call(bo r) { return r.cons.get_tail(); }
  };
  template <typename, typename I2>
  struct equal_to : ae<Cons, typename I2::cf> {};
};
struct boost_tuple_null_iterator : iterator_facade {
  typedef int cf;
  template <typename, typename> struct equal_to : aw::br<ae<int, int>> {};
};
template <> struct boost_tuple_iterator<> : boost_tuple_null_iterator {
  template <typename Cons> boost_tuple_iterator(Cons);
};
template <> struct boost_tuple_iterator<int const> : boost_tuple_null_iterator {
  template <typename Cons> boost_tuple_iterator(Cons) {}
};
template <> struct boost_tuple_iterator<tuple<>> : boost_tuple_null_iterator {};
template <typename> struct begin_impl {
  template <typename Sequence> struct apply {
    typedef boost_tuple_iterator<Sequence> h;
    static h call(Sequence r) { return r; }
  };
};
template <typename> struct end_impl {
  template <typename Sequence> struct apply {
    typedef boost_tuple_iterator<typename aw::cg<ba<Sequence>, int const>::h> h;
  };
};
template <typename> struct value_of_impl {
  template <typename bo> struct apply : bo::value_of<bo> {};
};
namespace result_of {
template <typename bo>
struct value_of : value_of_impl<typename bo::fusion_tag>::apply<bo> {};
} // namespace result_of
template <typename> struct deref_impl {
  template <typename bo> struct apply : bo::deref<bo> {};
};
namespace result_of {
template <typename bo>
struct deref : deref_impl<typename bo::fusion_tag>::apply<bo> {};
} // namespace result_of
template <typename bo> result_of::deref<bo>::h deref(bo r) {
  return result_of::deref<bo>::call(r);
}
template <typename bo> result_of::deref<bo>::h operator*(bo r) {
  return deref(r);
}
namespace detail {
template <typename First, typename Last,
          bool = result_of::equal_to<First, Last>::ax>
struct build_tuple_cons {
  typedef int h;
  static h call(First, Last) { return h(); }
};
template <typename First, typename Last>
struct build_tuple_cons<First, Last, false> {
  typedef build_tuple_cons<typename result_of::bh<First>::h, Last>
      next_build_tuple_cons;
  typedef tuples::cons<typename result_of::value_of<First>::h,
                       typename next_build_tuple_cons::h>
      h;
  static h call(First r, Last p2) {
    typename result_of::value_of<First>::h v = *r;
    return h(v, next_build_tuple_cons::call(bh(r), p2));
  }
};
} // namespace detail
namespace result_of {
template <typename Sequence>
struct begin
    : begin_impl<typename detail::tag_of<Sequence>::h>::apply<Sequence> {};
template <typename Sequence>
struct ab : end_impl<typename detail::tag_of<Sequence>::h>::apply<Sequence> {};
} // namespace result_of
namespace traits {
struct is_view : bb<0> {};
} // namespace traits
} // namespace fusion
namespace aw {
template <typename bo> struct fusion_iterator {
  typedef fusion::result_of::value_of<bo>::h h;
};
template <typename bo> struct bh<fusion_iterator<bo>> {
  typedef fusion_iterator<typename fusion::result_of::bh<bo>::h> h;
};
template <> struct begin_impl<int> {
  template <typename Sequence> struct apply {
    typedef fusion_iterator<typename fusion::result_of::begin<Sequence>::h> h;
  };
};
template <> struct end_impl<int> {
  template <typename Sequence> struct apply {
    typedef fusion_iterator<typename fusion::result_of::ab<Sequence>::h> h;
  };
};
} // namespace aw
namespace fusion {
template <> struct next_impl<int> {
  template <typename bo> struct apply {
    typedef aw::cg<
        result_of::equal_to<typename result_of::bh<typename bo::first_type>::h,
                            typename bo::last_type>,
        typename bo::concat_type>::h h;
  };
};
template <> struct value_of_impl<int> {
  template <typename bo> struct apply {
    typedef result_of::value_of<typename bo::first_type>::h h;
  };
};
template <typename First, typename Last, typename Concat>
struct joint_view_iterator {
  typedef First first_type;
  typedef Last last_type;
  typedef Concat concat_type;
  typedef int fusion_tag;
};
struct joint_view_tag;
template <> struct begin_impl<joint_view_tag> {
  template <typename Sequence> struct apply {
    typedef Sequence::first_type first_type;
    typedef Sequence::last_type last_type;
    typedef Sequence::concat_type concat_type;
    typedef aw::cg<result_of::equal_to<first_type, last_type>, concat_type,
                   joint_view_iterator<first_type, last_type, concat_type>>::h
        h;
  };
};
template <> struct end_impl<joint_view_tag> {
  template <typename Sequence> struct apply {
    typedef Sequence::concat_last_type h;
  };
};
template <typename Sequence1, typename Sequence2> struct joint_view {
  typedef joint_view_tag fusion_tag;
  typedef result_of::begin<Sequence1>::h first_type;
  typedef result_of::ab<Sequence1>::h last_type;
  typedef result_of::begin<Sequence2>::h concat_type;
  typedef result_of::ab<Sequence2>::h concat_last_type;
};
struct single_view_iterator_tag;
template <typename SingleView, typename Pos> struct single_view_iterator {
  typedef single_view_iterator_tag fusion_tag;
  typedef SingleView::value_type value_type;
  typedef Pos position;
  typedef SingleView single_view_type;
};
template <> struct next_impl<single_view_iterator_tag> {
  template <typename bo> struct apply {
    typedef single_view_iterator<typename bo::single_view_type,
                                 typename aw::bh<typename bo::position>::h>
        h;
  };
};
template <> struct value_of_impl<single_view_iterator_tag> {
  template <typename bo> struct apply {
    typedef bo::value_type h;
  };
};
struct single_view_tag;
template <> struct begin_impl<single_view_tag> {
  template <typename Sequence> struct apply {
    typedef single_view_iterator<Sequence, aw::bd<0>> h;
  };
};
template <> struct end_impl<single_view_tag> {
  template <typename Sequence> struct apply {
    typedef single_view_iterator<Sequence, aw::bd<1>> h;
  };
};
template <typename aq> struct single_view {
  typedef single_view_tag fusion_tag;
  typedef aq value_type;
};
namespace result_of {
template <typename Sequence, typename aq> struct push_back {
  typedef joint_view<Sequence, single_view<aq>> h;
};
} // namespace result_of
template <typename Sequence>
bu<result_of::begin<Sequence>>::h begin(Sequence r) {
  return result_of::begin<Sequence>::call(r);
}
template <typename Sequence> struct apply {
  typedef detail::build_tuple_cons<typename result_of::begin<Sequence>::h,
                                   typename result_of::ab<Sequence>::h>
      build_tuple_cons;
  typedef build_tuple_cons::h h;
  static h call(Sequence r) {
    typename result_of::begin<Sequence>::h __trans_tmp_6 = begin(r);
    typename result_of::ab<Sequence>::h __trans_tmp_7(r);
    return build_tuple_cons::call(__trans_tmp_6, __trans_tmp_7);
  }
};
namespace detail {
template <typename First, typename Last, typename df>
void for_each_linear(First, Last, df, bb<true>) {}
template <typename First, typename Last, typename df>
void for_each_linear(First r, Last p2, df p3, bb<false>) {
  p3(*r);
  for_each_linear(
      bh(r), p2, p3,
      result_of::equal_to<typename result_of::bh<First>::h, Last>());
}
template <typename Sequence, typename df, typename dn>
void for_each_dispatch(Sequence &r, df p2, dn) {
  typename result_of::begin<Sequence>::h __trans_tmp_2 = r;
  typename result_of::ab<Sequence>::h __trans_tmp_3 = 0;
  for_each_linear(__trans_tmp_2, __trans_tmp_3, p2,
                  result_of::equal_to<typename result_of::begin<Sequence>::h,
                                      typename result_of::ab<Sequence>::h>());
}
template <typename Sequence, typename df> void for_each(Sequence &r, df p2) {
  for_each_dispatch(r, p2, int());
}
} // namespace detail
template <typename df>
void for_each(tuple<join_iterator, join_iterator> &r, df p2) {
  detail::for_each(r, p2);
}
struct transform_view_iterator_tag;
template <> struct deref_impl<transform_view_iterator_tag> {
  template <typename bo> struct apply {
    typedef result_of::deref<typename bo::first_type>::h value_type;
    typedef bo::transform_type df;
    typedef typename result_of<df(value_type)>::h h;
    static h call(bo r) { return r.f(*r.ad); }
  };
};
template <typename First, typename df> struct transform_view_iterator {
  typedef transform_view_iterator_tag fusion_tag;
  typedef First first_type;
  typedef df transform_type;
  first_type ad;
  transform_type f;
};
template <> struct next_impl<transform_view_iterator_tag> {
  template <typename bo> struct apply {
    typedef transform_view_iterator<
        typename result_of::bh<typename bo::first_type>::h,
        typename bo::transform_type>
        h;
    static h call(bo r) { return h(bh(r.ad)); }
  };
};
template <> struct value_of_impl<transform_view_iterator_tag> {
  template <typename bo> struct apply {
    typedef av::result_of<typename bo::transform_type(
        typename result_of::value_of<typename bo::first_type>::h)>::h h;
  };
};
struct transform_view_tag;
template <> struct begin_impl<transform_view_tag> {
  template <typename Sequence> struct apply {
    typedef transform_view_iterator<typename Sequence::first_type,
                                    typename Sequence::transform_type>
        h;
    static h call(Sequence r) { return h(r.ad()); }
  };
};
template <> struct end_impl<transform_view_tag> {
  template <typename Sequence> struct apply {
    typedef transform_view_iterator<typename Sequence::last_type,
                                    typename Sequence::transform_type>
        h;
  };
};
} // namespace fusion
namespace aw {
template <> struct clear_impl<int> {
  template <typename> struct apply {
    typedef tuple<> h;
  };
};
} // namespace aw
namespace fusion {
namespace result_of {
template <typename Sequence> struct convert {
  typedef apply<Sequence> gen;
  typedef gen::h h;
};
} // namespace result_of
template <typename, typename Sequence>
result_of::convert<Sequence>::h convert(Sequence r) {
  typedef typename result_of::convert<Sequence>::gen gen;
  return gen::call(r);
}
} // namespace fusion
namespace aw {
template <> struct push_back_impl<int> {
  template <typename Sequence, typename aq> struct apply {
    typedef fusion::result_of::convert<
        typename fusion::result_of::push_back<Sequence, aq>::h>::h h;
  };
};
} // namespace aw
namespace fusion {
template <typename, typename df> struct transform_view {
  typedef transform_view_tag fusion_tag;
  typedef result_of::begin<const tuple<join_iterator, join_iterator>>::h
      first_type;
  typedef result_of::ab<const tuple<>>::h last_type;
  typedef df transform_type;
  first_type ad() { return seq; }
  aw::cg<traits::is_view, int, const tuple<join_iterator, join_iterator> &>::h
      seq;
};
namespace result_of {
template <typename, typename df> struct transform {
  typedef transform_view<const tuple<join_iterator, join_iterator>, df> h;
};
} // namespace result_of
template <typename df>
result_of::transform<int, df>::h
transform(const tuple<join_iterator, join_iterator> &r, df) {
  return transform_view<const tuple<join_iterator, join_iterator>, df>(r);
}
namespace detail {
struct sequence_equal_to {
  template <typename I1, typename I2> static bool call(I1, I2, bb<true>);
  template <typename I1, typename I2> static bool call(I1 r, I2 p2, bb<false>) {
    return *r == *p2 && call(bh(r), bh(p2));
  }
  template <typename I1, typename I2> static bool call(I1 r, I2 p2) {
    result_of::equal_to<I1, result_of::ab<tuple<>>::h> eq;
    return call(r, p2, eq);
  }
};
} // namespace detail
template <typename Seq2>
bool equal_to(const tuple<join_iterator, join_iterator> &r, Seq2 p2) {
  boost_tuple_iterator __trans_tmp_27 = r;
  return detail::sequence_equal_to::call(__trans_tmp_27, begin(p2));
}
} // namespace fusion
namespace iterators {
struct increment_iterator {
  template <typename bo> void operator()(bo &r) { ++r; }
};
struct dereference_iterator {
  template <typename> struct result;
  template <typename This, typename bo> struct result<This(bo)> {
    typedef by<bo>::h h;
  };
  template <typename bo> result<dereference_iterator(bo)>::h operator()(bo r) {
    return *r;
  }
};
struct minimum_traversal_category_in_iterator_tuple;
typedef aw::transform<tuple<join_iterator, join_iterator>>::h ah;
static ah call(fusion::transform_view<const tuple<join_iterator, join_iterator>,
                                      dereference_iterator>
                   r) {
  return convert < ah >> (r);
}
template <typename>
struct zip_iterator
    : iterator_facade<zip_iterator<int>, ah,
                      minimum_traversal_category_in_iterator_tuple, ah> {
  const tuple<join_iterator, join_iterator> &get_iterator_tuple() const {
    return m_iterator_tuple;
  }
  ah dereference() {
    const tuple<join_iterator, join_iterator> &__trans_tmp_21 =
        get_iterator_tuple();
    fusion::transform_view __trans_tmp_26 =
        fusion::transform(__trans_tmp_21, dereference_iterator());
    return call(__trans_tmp_26);
  }
  template <typename OtherIteratorTuple> bool equal(OtherIteratorTuple) const {
    const tuple<join_iterator, join_iterator> &__trans_tmp_24 =
        get_iterator_tuple();
    tuple<join_iterator, join_iterator> __trans_tmp_25;
    return fusion::equal_to(__trans_tmp_24, __trans_tmp_25);
  }
  void increment() { fusion::for_each(m_iterator_tuple, increment_iterator()); }
  tuple<join_iterator, join_iterator> m_iterator_tuple;
};
} // namespace iterators
using iterators::zip_iterator;
} // namespace av
struct Construct_curve_2 {
  template <typename ForwardIterator>
  void operator()(ForwardIterator r, ForwardIterator p2) const {
    constructor_impl(r, p2, int());
  }
  void operator()(int) {
    av::join_iterator __trans_tmp_11, __trans_tmp_18;
    operator()(__trans_tmp_18, __trans_tmp_11);
  }
  template <typename ForwardIterator>
  void constructor_impl(ForwardIterator r, ForwardIterator p2, b<false>) const {
    Arr_polycurve_traits_2::Construct_curve_2()(r, p2);
  }
  void constructor_impl(av::join_iterator, av::join_iterator, b<true>) const {
    auto point_pair_to_segment = [](av::zip_iterator<av::tuple<>>::ah t) {
      construct_curve_2_object()(0, get<1>(t));
    };
    av::zip_iterator<int> __trans_tmp_20, __trans_tmp_22;
    operator()(make_transform_iterator(__trans_tmp_22, point_pair_to_segment),
               make_transform_iterator(__trans_tmp_20, point_pair_to_segment));
  }
};
int __trans_tmp_33___trans_tmp_16;
int
__trans_tmp_33()
{
  Construct_curve_2 __trans_tmp_10;
  __trans_tmp_10(__trans_tmp_33___trans_tmp_16);
}

// { dg-final { scan-assembler-times "call\[ \t\]__tls_get_addr@PLT" 1 { target { ! ia32 } } } }
