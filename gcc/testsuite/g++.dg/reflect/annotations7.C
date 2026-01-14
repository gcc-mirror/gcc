// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

struct U {
  constexpr U() = default;
  U(const U&) = delete;
};
constexpr U u;
struct [[ =u ]] deletedCopy{};		// { dg-error "annotation does not have copy constructible type" }
					// { dg-error "use of deleted function 'U::U\\\(const U\\\&\\\)'" "" { target *-*-* } .-1 }
struct [[ =U{} ]] deletedCopy2{};	// { dg-error "annotation does not have copy constructible type" }
					// { dg-error "use of deleted function 'U::U\\\(const U\\\&\\\)'" "" { target *-*-* } .-1 }
