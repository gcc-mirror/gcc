// PR c++/104646
// { dg-do compile { target c++11 } }
// { dg-additional-options -fno-elide-constructors }

template <typename _T1> struct pair {
  _T1 first;
  int second;
};
template <typename _Iterator> class __normal_iterator {
  _Iterator __traits_type;

public:
  constexpr __normal_iterator() {}
};
template <typename> class allocator;
template <typename> struct allocator_traits;
template <typename _Tp> struct allocator_traits<allocator<_Tp>> {
  using value_type = _Tp;
  template <typename _Up> using rebind_alloc = allocator<_Up>;
};
template <typename _Alloc> struct __alloc_traits {
  typedef allocator_traits<_Alloc> _Base_type;
  typedef typename _Base_type::value_type &const_reference;
  template <typename _Tp> struct rebind {
    typedef typename _Base_type::template rebind_alloc<_Tp> other;
  };
};
template <typename _Tp, typename _Alloc> struct _Vector_base {
  typedef typename __alloc_traits<_Alloc>::template rebind<_Tp>::other _Tp_alloc_type;
};
template <typename _Tp, typename _Alloc = allocator<_Tp>> class vector {
public:
  typename __alloc_traits<
      typename _Vector_base<_Tp, _Alloc>::_Tp_alloc_type>::const_reference
  operator[](long);
};
enum match_flag_type {};
template <typename, typename> class Trans_NS___cxx11_basic_regex;
class Trans_NS___cxx11_match_results;
enum _RegexExecutorPolicy { _S_auto };
template <typename, typename, typename _CharT, typename _TraitsT,
          _RegexExecutorPolicy, bool>
bool __regex_algo_impl(Trans_NS___cxx11_match_results &,
                       const Trans_NS___cxx11_basic_regex<_CharT, _TraitsT> &);
template <typename, typename, typename, bool> class _Executor;
template <typename _Ch_type, typename = _Ch_type>
class Trans_NS___cxx11_basic_regex {};
class Trans_NS___cxx11_match_results : vector<int> {
  template <typename, typename, typename _Cp, typename _Rp,
            _RegexExecutorPolicy, bool>
  friend bool __regex_algo_impl(Trans_NS___cxx11_match_results &,
                                const Trans_NS___cxx11_basic_regex<_Cp, _Rp> &);
};
template <typename _Bi_iter, typename _Alloc, typename _Ch_type,
          typename _Rx_traits>
void regex_search(_Bi_iter, _Alloc,
                  Trans_NS___cxx11_basic_regex<_Ch_type, _Rx_traits>) {
  __regex_algo_impl<_Bi_iter, _Alloc, _Ch_type, _Rx_traits, _S_auto, false>;
}
match_flag_type __regex_algo_impl___flags;
template <typename, typename, typename _CharT, typename _TraitsT,
          _RegexExecutorPolicy, bool>
bool __regex_algo_impl(
    Trans_NS___cxx11_match_results &__m,
    const Trans_NS___cxx11_basic_regex<_CharT, _TraitsT> &__re) {
  __normal_iterator<const char *> __e, __s;
  _Executor<int, int, _TraitsT, false> __executor(__s, __e, __m, __re,
                                                  __regex_algo_impl___flags);
  __executor._M_match();
  return false;
}
template <typename, typename, typename, bool> class _Executor {
public:
  _Executor(__normal_iterator<const char *>, __normal_iterator<const char *>,
            vector<int>, Trans_NS___cxx11_basic_regex<char>, match_flag_type);
  void _M_match() { _M_dfs(); }
  void _M_dfs();
  vector<pair<__normal_iterator<char *>>> _M_rep_count;
};
long _M_rep_once_more___i;
template <typename _BiIter, typename _Alloc, typename _TraitsT, bool __dfs_mode>
void _Executor<_BiIter, _Alloc, _TraitsT, __dfs_mode>::_M_dfs() {
  auto __rep_count = _M_rep_count[_M_rep_once_more___i];
}
char main___trans_tmp_1;
void main___trans_tmp_2() {
  Trans_NS___cxx11_basic_regex<char> re;
  regex_search(main___trans_tmp_1, main___trans_tmp_2, re);
}
