// PR c++/103408
// { dg-do compile { target c++23 } }

template<int>
concept C = auto([]{}); // { dg-error "constraint" }
static_assert(C<0>); // { dg-error "non-constant condition for static assertion" }
