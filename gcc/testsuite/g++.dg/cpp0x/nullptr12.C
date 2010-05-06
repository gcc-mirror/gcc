// { dg-do compile }
// { dg-options "-std=c++0x" }

// Test sizeof

static_assert(sizeof(nullptr) == sizeof(void*), "sizeof(nullptr) is wrong");
