// P2786R13 - C++26 Trivial Relocatability
// { dg-do compile { target c++11 } }
// { dg-options "" }
// { dg-additional-options "-pedantic" { target c++17 } }

namespace std
{
template <typename T, T v>
struct integral_constant
{
  static constexpr T value = v;
};

template <typename T>
inline constexpr bool is_trivially_relocatable_v	// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  = __builtin_is_trivially_relocatable (T);		// { dg-warning "variable templates only available with" "" { target c++11_down } .-1 }
							// { dg-error "invalid use of incomplete type 'struct A'" "" { target *-*-* } .-1 }

template <typename T>
inline constexpr bool is_nothrow_relocatable_v		// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  = __builtin_is_nothrow_relocatable (T);		// { dg-warning "variable templates only available with" "" { target c++11_down } .-1 }
							// { dg-error "invalid use of incomplete type 'struct A'" "" { target *-*-* } .-1 }

template <typename T>
inline constexpr bool is_replaceable_v			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  = __builtin_is_replaceable (T);			// { dg-warning "variable templates only available with" "" { target c++11_down } .-1 }
}							// { dg-error "invalid use of incomplete type 'struct A'" "" { target *-*-* } .-1 }

struct A;						// { dg-message "forward declaration of 'struct A'" }

auto a = std::is_trivially_relocatable_v <A>;		// { dg-message "required from here" }
auto b = std::is_nothrow_relocatable_v <A>;		// { dg-message "required from here" }
auto c = std::is_replaceable_v <A>;			// { dg-message "required from here" }
