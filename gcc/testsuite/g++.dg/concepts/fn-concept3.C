// PR c++/92746
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T> concept C3 = true;
static_assert(noexcept(C3<int>), "concept should be treated as if noexcept(true) specified");
