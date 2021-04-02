
template<class _CharT> struct char_traits;

template<typename _CharT, typename _Traits = char_traits<_CharT>>
class basic_string;

typedef basic_string<char> string;

template<typename> struct iterator_traits;

template<bool _Cond> struct conditional;

template<typename _Iter>
inline constexpr bool disable_sized_sentinel_for = false;

template<typename _Iter>
concept sized_sentinel_for = !disable_sized_sentinel_for<_Iter>;

template<typename _Iterator>
class __normal_iterator
{
  typedef iterator_traits<_Iterator> __traits_type;
};

template<typename _Iterator>
class reverse_iterator
{
public:
  using iterator_concept
    = typename conditional<sized_sentinel_for<_Iterator>>::type;
};


template<typename _Iterator>
requires (!sized_sentinel_for<_Iterator>)
bool disable_sized_sentinel_for<reverse_iterator<_Iterator>> = true;


template<typename _Iterator>
bool operator==(const reverse_iterator<_Iterator>& __x,
		const reverse_iterator<_Iterator>& __y);
template<typename _Iterator>
bool operator==(const __normal_iterator<_Iterator>& __lhs,
		const __normal_iterator<_Iterator>& __rhs);

template<typename _It >
class common_iterator
{
public:
  friend bool operator==(const common_iterator& __x,
			 const common_iterator& __y)
  {
    return __x._M_it == __y._M_it;
  }

private:
  _It _M_it;
};

template<typename _It>
struct iterator_traits<common_iterator<_It>>
{
};

template<typename _CharT>
struct char_traits
{
  static bool eq(const _CharT& __c1, const _CharT& __c2)
  { return __c1 == __c2; }
};
