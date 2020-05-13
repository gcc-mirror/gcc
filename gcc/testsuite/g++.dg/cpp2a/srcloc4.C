// { dg-do compile { target c++20 } }

namespace std {
  void source_location ();
}

auto x = __builtin_source_location ();	// { dg-error "'void std::source_location\\(\\)' is not a type" }
// { dg-message "evaluating '__builtin_source_location'" "" { target *-*-* } .-1 }
