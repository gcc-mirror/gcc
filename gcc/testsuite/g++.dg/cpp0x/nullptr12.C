// { dg-do compile }
// { dg-options "-std=c++0x" }

// Test sizeof

static_assert(sizeof(nullptr) == sizeof(void*), "sizeof(nullptr) is wrong");
const decltype(nullptr) mynull = 0;
static_assert(sizeof(mynull) == sizeof(void*), "sizeof(nullptr) is wrong");
