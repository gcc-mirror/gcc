// PR c++/119844
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }

export module M;

struct S { int value; };

export struct A { int value; };
export using B = S;  // typedef, shouldn't correspond
export template <typename T> struct C { int value; };  // template vs. non-template

// we use static_assert(false) to ensure we don't try to complete the body
// and get unrelated errors while reporting
export template <typename T> struct D { static_assert(false); };
export template <typename T> using E = S;  // typedef, shouldn't correspond

export template <typename T> struct F;
template <> struct F<int> { int value; };

export template <typename T> struct G { static_assert(false); };

export template <typename T> struct H;
template <typename T> struct H<const T> { static_assert(false); };
#if __cpp_concepts >= 201907L
template <typename T> requires true struct H<const T> { static_assert(false); };
#endif
