// { dg-do compile }
// { dg-additional-options "-std=gnu++20 -O2 -w -march=isaa" }
namespace std {
struct __conditional {
  template <typename _Tp, typename> using type = _Tp;
};
template <bool, typename _If, typename _Else>
using __conditional_t = __conditional::type<_If, _Else>;
template <typename> constexpr bool is_lvalue_reference_v = true;
template <typename> bool is_same_v;
template <typename _From, typename _To>
constexpr bool is_convertible_v = __is_convertible(_From, _To);
template <typename> bool is_invocable_v;
template <typename...> using common_reference_t = int;
template <typename _Iterator> struct iterator_traits : _Iterator {};
namespace __detail {
template <typename, typename _Up>
concept __same_as = is_same_v<_Up>;
}
template <typename _Tp, typename _Up>
concept same_as = __detail::__same_as<_Up, _Tp>;
template <typename _Derived, typename _Base>
concept derived_from = is_convertible_v<_Derived, _Base>;
template <typename, typename>
concept common_reference_with =
    same_as<common_reference_t<>, common_reference_t<>>;
namespace __detail {
template <typename _Tp>
concept __boolean_testable = requires(_Tp __t) { __t; };
template <typename _Tp, typename>
concept __weakly_eq_cmp_with = requires(_Tp __t) { __t; };
} // namespace __detail
template <typename... _Args>
concept invocable = is_invocable_v<_Args...>;
template <typename>
concept regular_invocable = invocable<>;
template <typename _Fn>
concept predicate = __detail::__boolean_testable<_Fn>;
template <typename _Rel, typename, typename>
concept relation = predicate<_Rel>;
template <typename _Rel, typename _Tp, typename _Up>
concept strict_weak_order = relation<_Rel, _Tp, _Up>;
namespace {
struct duration {
  duration() = default;
  template <typename _Rep2> duration(_Rep2 __rep) : __r(__rep) {}
  long long count() { return __r; }
  long long __r;
};
long long operator+(duration __lhs, duration __rhs) {
  long __trans_tmp_1 = __lhs.count();
  return __trans_tmp_1 + __rhs.count();
}
struct time_point {
  time_point();
  time_point(duration __dur) : __d(__dur) {}
  duration time_since_epoch() { return __d; }
  duration __d;
};
time_point operator+(time_point __lhs, duration __rhs) {
  duration __trans_tmp_2 = __lhs.time_since_epoch();
  return time_point(__trans_tmp_2 + __rhs);
}
template <typename> using sys_time = time_point;
} // namespace
template <typename> class allocator;
namespace {
struct less {};
} // namespace
struct forward_iterator_tag {};
namespace __detail {
template <typename _Iter> struct __iter_traits_impl {
  using type = iterator_traits<_Iter>;
};
template <typename _Iter> using __iter_traits = __iter_traits_impl<_Iter>::type;
template <typename> struct __iter_concept_impl;
template <typename _Iter>
  requires requires { typename _Iter; }
struct __iter_concept_impl<_Iter> {
  using type = __iter_traits<_Iter>::iterator_concept;
};
template <typename _Iter>
using __iter_concept = __iter_concept_impl<_Iter>::type;
template <typename _In>
concept __indirectly_readable_impl = common_reference_with<_In, _In>;
} // namespace __detail
template <typename _In>
concept indirectly_readable = __detail::__indirectly_readable_impl<_In>;
template <typename _Iter>
concept input_or_output_iterator = requires(_Iter __i) { __i; };
template <typename _Sent, typename _Iter>
concept sentinel_for = __detail::__weakly_eq_cmp_with<_Sent, _Iter>;
template <typename _Iter>
concept forward_iterator =
    derived_from<__detail::__iter_concept<_Iter>, forward_iterator_tag>;
template <typename _Fn, typename>
concept indirectly_regular_unary_invocable = regular_invocable<_Fn>;
template <typename _Fn, typename _I1, typename _I2>
concept indirect_strict_weak_order = strict_weak_order<_Fn, _I1, _I2>;
namespace __detail {
template <typename, typename> struct __projected;
}
template <indirectly_readable _Iter,
          indirectly_regular_unary_invocable<_Iter> _Proj>
using projected = __detail::__projected<_Iter, _Proj>;
namespace ranges::__access {
template <typename _Tp> auto __begin(_Tp __t) { return __t.begin(); }
} // namespace ranges::__access
namespace __detail {
template <typename _Tp>
using __range_iter_t = decltype(ranges::__access::__begin(_Tp()));
}
template <typename _Tp> struct iterator_traits<_Tp *> {
  typedef forward_iterator_tag iterator_concept;
  using reference = _Tp;
};
} // namespace std
template <typename _Iterator, typename> struct __normal_iterator {
  using iterator_concept = std::__detail::__iter_concept<_Iterator>;
  std::iterator_traits<_Iterator>::reference operator*();
  void operator++();
};
template <typename _IteratorL, typename _IteratorR, typename _Container>
bool operator==(__normal_iterator<_IteratorL, _Container>,
                __normal_iterator<_IteratorR, _Container>);
int _M_get_sys_info_ri;
namespace std {
template <typename> struct allocator_traits;
template <typename _Tp> struct allocator_traits<allocator<_Tp>> {
  using pointer = _Tp *;
};
namespace {
namespace __detail {
template <typename _Tp>
concept __maybe_borrowed_range = is_lvalue_reference_v<_Tp>;
}
int end;
template <typename>
concept range = requires { end; };
template <typename _Tp>
concept borrowed_range = __detail::__maybe_borrowed_range<_Tp>;
template <typename _Tp> using iterator_t = std::__detail::__range_iter_t<_Tp>;
template <typename _Tp>
concept forward_range = forward_iterator<iterator_t<_Tp>>;
struct dangling;
template <input_or_output_iterator _It, sentinel_for<_It> _Sent = _It>
struct subrange {
  _It begin();
  _Sent end();
};
template <range _Range>
using borrowed_subrange_t =
    __conditional_t<borrowed_range<_Range>, subrange<iterator_t<_Range>>,
                    dangling>;
} // namespace
template <typename _Alloc> struct _Vector_base {
  typedef allocator_traits<_Alloc>::pointer pointer;
};
template <typename _Tp, typename _Alloc = allocator<_Tp>> struct vector {
  __normal_iterator<typename _Vector_base<_Alloc>::pointer, int> begin();
};
template <typename _Tp> struct __shared_ptr_access {
  _Tp *operator->();
};
namespace chrono {
struct weekday {
  char _M_wd;
  weekday _S_from_days() {
    long __trans_tmp_3;
    return 7 > __trans_tmp_3;
  }
  weekday(unsigned __wd) : _M_wd(__wd == 7 ?: __wd) {}
  weekday() : weekday{_S_from_days()} {}
  unsigned c_encoding() { return _M_wd; }
  friend duration operator-(weekday __x, weekday __y) {
    auto __n = __x.c_encoding() - __y.c_encoding();
    return int(__n) >= 0 ? __n : duration{};
  }
};
struct year_month_day {
  year_month_day _S_from_days(duration __dp) {
    auto __r0 = int(__dp.count()), __u2 = 5 * __r0;
    year_month_day{__u2};
  }
  year_month_day();
  year_month_day(int);
  year_month_day(sys_time<long> __dp)
      : year_month_day(_S_from_days(__dp.time_since_epoch())) {}
};
struct time_zone {
  void _M_get_sys_info() const;
};
} // namespace chrono
namespace ranges {
struct {
  template <
      forward_range _Range, typename _Tp, typename _Proj,
      indirect_strict_weak_order<_Tp, projected<_Range, _Proj>> _Comp = less>
  borrowed_subrange_t<_Range> operator()(_Range, _Tp, _Comp, _Proj);
} equal_range;
} // namespace ranges
} // namespace std
namespace std::chrono {
struct Rule;
unsigned day_of_week;
struct _Node {
  vector<Rule> rules;
};
char name;
struct Rule {
  int from;
  void start_time(int, long) {
    year_month_day ymd;
    sys_time<long> date;
    duration diff = day_of_week - weekday{};
    ymd = date + diff;
  }
};
__shared_ptr_access<_Node> __trans_tmp_5;
void time_zone::_M_get_sys_info() const {
  auto node = __trans_tmp_5;
  auto rules = ranges::equal_range(node->rules, _M_get_sys_info_ri, {}, name);
  for (auto rule : rules)
    rule.start_time(rule.from, {});
}
} // namespace std::chrono
