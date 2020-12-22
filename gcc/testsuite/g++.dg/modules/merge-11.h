


template<typename _From, bool>
struct __is_nt_convertible_helper;

template<typename _From>
class __is_nt_convertible_helper<_From, false>
{
  template<typename> static int __test (int);
  template<typename> static void __test(...);
  
public:
  using type = decltype(__test<_From>(0));
};
