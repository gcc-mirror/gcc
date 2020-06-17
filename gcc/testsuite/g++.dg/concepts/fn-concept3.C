// PR c++/92746
// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts" }

template<typename T> concept bool C3() { return true; }
static_assert(noexcept(C3<int>()), "function concept should be treated as if noexcept(true) specified");
