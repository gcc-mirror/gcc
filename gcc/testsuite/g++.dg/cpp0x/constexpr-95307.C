// PR c++/95307
// { dg-do compile { target c++11 } }

int v;
constexpr auto p{reinterpret_cast<__UINTPTR_TYPE__>(&v) - 1u};	// { dg-error "conversion from pointer type" }
