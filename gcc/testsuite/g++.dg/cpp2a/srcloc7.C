// { dg-do compile { target c++20 } }

namespace std {
  struct source_location {
    static void __impl ();
  };
}

auto x = __builtin_source_location ();	// { dg-error "'std::source_location::__impl\\(\\)' is not a type" }
// { dg-message "evaluating '__builtin_source_location'" "" { target *-*-* } .-1 }
