template < typename T > struct integral_c {
  static const T value = 0;
};
struct is_reference:integral_c < bool > { };
template < class > struct is_function_ptr_helper { };
template < bool > struct is_function_chooser;

template <> struct is_function_chooser <0 >
{
  template < typename T > struct result_:is_function_ptr_helper < T * > { };
};

template < typename T > struct is_function_impl:is_function_chooser <
  is_reference::value >::result_ < T > { };
