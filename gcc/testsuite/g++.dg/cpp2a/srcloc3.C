// { dg-do compile { target c++2a } }

auto x = __builtin_source_location ();	// { dg-error "'source_location' is not a member of 'std'" }
// { dg-message "std::source_location' is defined in header '<source_location>'; did you forget to '#include <source_location>'" "" { target *-*-* } .-1 }
// { dg-message "evaluating '__builtin_source_location'" "" { target *-*-* } .-2 }
