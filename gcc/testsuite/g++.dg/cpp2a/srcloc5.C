// { dg-do compile { target c++2a } }

namespace std {
  typedef int source_location;
}

auto x = __builtin_source_location ();	// { dg-error "'std::source_location'\[^\n\r]*is not a class type" }
// { dg-error "'__impl' is not a member of 'std::source_location'" "" { target *-*-* } .-1 }
// { dg-message "evaluating '__builtin_source_location'" "" { target *-*-* } .-2 }
