// { dg-do compile { target c++20 } }

namespace std {
  struct source_location {
    typedef int __impl;
  };
}

auto x = __builtin_source_location ();	// { dg-error "'std::source_location::__impl()' is not a class type" }
// { dg-message "evaluating '__builtin_source_location'" "" { target *-*-* } .-1 }
