template<bool __v>
struct integral_constant {};

typedef integral_constant<true> true_type;
typedef integral_constant<false> false_type;

template<typename>
struct __is_not_void_helper
  : public true_type { };

template<> struct __is_not_void_helper<void>
  : public false_type { };

template<typename _Tp, typename _Up>
struct is_same
  : public integral_constant<__is_same_as(_Tp, _Up)> {};

template<typename _Iterator>
struct iterator_traits;

template<typename _Tp> requires __is_not_void_helper<_Tp>::value
struct iterator_traits<_Tp*>;

template<typename _Tp>
struct iterator {};

template<typename _Iterator>
class reverse_iterator
  : public iterator<typename iterator_traits<_Iterator>::value_type>
{
};

template<typename _Tp> struct _Deque_iterator;

template<typename _Tp> void __copy_move_a1(_Deque_iterator<_Tp>);

template<typename _Iterator>
decltype (NOPE (_Iterator{})) __niter_base (reverse_iterator<_Iterator> __it);


template<typename _II>
inline void __copy_move_a (_II __first)
{
  __copy_move_a1 (__niter_base (__first));
}

template<typename _CharT>
struct __gnu_char_traits
{
  static void move (const char* __s2)
  {
    __copy_move_a (__s2);
  }
};

class string_view
{
  using t = __gnu_char_traits<char>;

public:
    string_view (const char* __str) noexcept;
};

template<typename _Ret>
void __stoa ()
{
  struct _Range_chk {
    static bool _S_chk (false_type) { return false; }
    static bool _S_chk (true_type) { return true; }
  };

  _Range_chk::_S_chk (is_same<_Ret, long>{});
}

inline void stoi () { __stoa<long> (); }
