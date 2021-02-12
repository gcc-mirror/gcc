// PR c++/97742
// { dg-do compile { target c++20 } }

template <int = requires { true	// { dg-error "expected" }
