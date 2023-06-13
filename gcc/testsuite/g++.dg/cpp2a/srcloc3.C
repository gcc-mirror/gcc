// { dg-do compile { target c++20 } }

auto x = __builtin_source_location ();	// { dg-error "'source_location' is not a member of 'std'" }
// { dg-message "std::source_location' is defined in header '<source_location>'; this is probably fixable by adding '#include <source_location>'" "" { target *-*-* } .-1 }
// { dg-message "using '__builtin_source_location'" "" { target *-*-* } .-2 }
