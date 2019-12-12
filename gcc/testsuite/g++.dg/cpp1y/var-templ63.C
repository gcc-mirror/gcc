// PR c++/91740 - ICE with constexpr call and ?: in ARRAY_REF.
// { dg-do compile { target c++14 } }

constexpr bool f(const char*) { return true; }
template<typename T> const char c = "FOO"[f("BAR") ? 1 : 0];
