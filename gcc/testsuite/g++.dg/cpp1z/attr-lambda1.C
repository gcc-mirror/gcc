// PR c++/89640
// { dg-options "-Wno-attributes" }
// { dg-do compile { target c++17 } }

void test() {
    []() mutable [[gnu::cold]] constexpr {}();		// { dg-error "expected" }
    []() constexpr [[gnu::cold]] mutable {}();		// { dg-error "expected" }
    []() [[gnu::cold]] mutable constexpr {}();		// { dg-error "expected" }
    []() [[gnu::cold]] constexpr mutable {}();		// { dg-error "expected" }
    []() mutable constexpr [[gnu::cold]] {}();
    []() constexpr mutable [[gnu::cold]] {}();
}
