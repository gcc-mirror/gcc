// { dg-do compile { target c++11 } }

#include <cstddef>
#include <type_traits>

std::size_t s1 = 1234zu; // { dg-warning {use of C\+\+23 .size_t. integer constant} "" { target c++20_down } }
std::size_t S1 = 5678ZU; // { dg-warning {use of C\+\+23 .size_t. integer constant} "" { target c++20_down } }
std::size_t s2 = 1234uz; // { dg-warning {use of C\+\+23 .size_t. integer constant} "" { target c++20_down } }
std::size_t S2 = 5678UZ; // { dg-warning {use of C\+\+23 .size_t. integer constant} "" { target c++20_down } }

std::make_signed<std::size_t>::type pd1 = 1234z; // { dg-warning {use of C\+\+23 .make_signed_t<size_t>. integer constant} "" { target c++20_down } }
std::make_signed<std::size_t>::type PD1 = 5678Z; // { dg-warning {use of C\+\+23 .make_signed_t<size_t>. integer constant} "" { target c++20_down } }
