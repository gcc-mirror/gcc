// PR c++/126867
// { dg-do compile { target c++17 } }

template <class T>
constexpr bool U = __has_unique_object_representations (T);

struct S {};		// { dg-message "'S' does not have unique object representations, because" }
			// { dg-message "'S' has padding and no data fields" "" { target *-*-* } .-1 }

static_assert (U <S>);	// { dg-error "static assertion failed" }
