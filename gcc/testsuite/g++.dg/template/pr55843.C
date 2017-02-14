// { dg-options "-ftemplate-depth-8" }

template< typename T > struct type_wrapper {
};
typedef char (&yes_tag)[2];
template<bool b> struct if_c {
};
template< typename T > struct has_type {
  struct gcc_3_2_wknd {
    template< typename U > static yes_tag test( type_wrapper<U> const volatile* // { dg-message "required" }
, type_wrapper<typename U::type>* = 0 );
  };
  typedef type_wrapper<T> t_;
  static const bool value = sizeof(gcc_3_2_wknd::test(static_cast<t_*>(0))) == // { dg-message "required" }
sizeof(yes_tag);
};
template <class K, class T, class=void> struct Get_type {
};
struct FT_tag {};
struct RT_tag {};
template <class K> struct Get_type<K, RT_tag, typename if_c<
!has_type<Get_type<K, FT_tag> >::value >::type> { }; // { dg-message "required" }
template <class K> struct Get_type<K, FT_tag, typename if_c<
!has_type<Get_type<K, RT_tag> >::value >::type> { };  // { dg-message "required" }
typedef Get_type<int, FT_tag>::type P;

// { dg-prune-output "-ftemplate-depth" }
// { dg-prune-output "compilation terminated" }
