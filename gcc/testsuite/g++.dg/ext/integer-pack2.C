// { dg-do compile { target { c++11 && int32 } } }
// { dg-options -w }

template<typename T, T...> struct integer_sequence { };
template<typename T, T num>
 using make_integer_sequence = integer_sequence<T, __integer_pack(num)...>; // { dg-error "argument" }

#if __SIZEOF_LONG_LONG__ > __SIZEOF_INT__
make_integer_sequence<int, 1LL << (__SIZEOF_INT__ * __CHAR_BIT__)> w;
#endif
make_integer_sequence<int, __INT_MAX__> x; // { dg-message "required" }
make_integer_sequence<int, -2147483650> y; // { dg-message "required" }
