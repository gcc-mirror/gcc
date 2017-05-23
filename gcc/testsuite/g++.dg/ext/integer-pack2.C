// { dg-do compile { target c++11 } }
// { dg-options -w }

#include <limits.h>

template<typename T, T...> struct integer_sequence { };
template<typename T, T num>
 using make_integer_sequence = integer_sequence<T, __integer_pack(num)...>; // { dg-error "argument" }

make_integer_sequence<int, -9223372036854775808> w;
make_integer_sequence<int, INT_MAX> x;	   // { dg-message "required" }
make_integer_sequence<int, -2147483650> y; // { dg-message "required" }
