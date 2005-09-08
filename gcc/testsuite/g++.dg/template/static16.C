// PR c++/23691

namespace std {
  class type_info {
    bool operator==(const type_info& __arg) const;
  };
}
template <class T, T val> struct integral_constant {
  static const T value = val;
};
template< typename T > struct is_integral : integral_constant<bool,false> {};
template <bool B>   struct enable_if_c {};
template<typename Functor>
typename enable_if_c<(is_integral<Functor>::value)>::type
operator==(const int& f, Functor g);
template<class D>
int get_deleter( std::type_info const & ti )
{
  return ti == typeid(D);
}
