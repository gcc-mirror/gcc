template<bool, typename, typename>
struct conditional;

template<typename> struct incrementable_traits;

template<typename _Iter, typename _Tp>
struct __iter_traits_impl;

class __max_diff_type;

template<typename _Iter>
concept weakly_incrementable
  =  __is_same (__iter_traits_impl<_Iter, incrementable_traits<_Iter>>,
		__max_diff_type);

template<typename _Iterator>
class reverse_iterator
{
public:
  using iterator_concept
    = typename conditional<weakly_incrementable<_Iterator>,
			   int, int>::type;
};
