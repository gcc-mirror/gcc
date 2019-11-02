// PR c++/89640
// { dg-options "-Wno-attributes" }
// { dg-do compile { target c++17 } }

void test() {
    []() mutable __attribute__((cold)) constexpr {}();	// { dg-error "expected" }
    []() constexpr __attribute__((cold)) mutable {}();	// { dg-error "expected" }
    []() __attribute__((cold)) mutable constexpr {}();	// { dg-error "expected" }
    []() __attribute__((cold)) constexpr mutable {}();	// { dg-error "expected" }
    []() mutable constexpr __attribute__((cold)) {}();
    []() constexpr mutable __attribute__((cold)) {}();
}
