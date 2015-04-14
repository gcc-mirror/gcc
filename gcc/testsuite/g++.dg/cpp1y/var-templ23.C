// PR c++/65625
// { dg-do compile { target c++14 } }

namespace std {
  template <typename T> int declval;
  typename std::declval<> d;	// { dg-error "type" }
}

// { dg-prune-output "expected" }
