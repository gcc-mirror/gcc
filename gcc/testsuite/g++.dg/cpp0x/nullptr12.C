// { dg-do compile { target c++11 } }

// Test sizeof

static_assert(sizeof(nullptr) == sizeof(void*), "sizeof(nullptr) is wrong");
const decltype(nullptr) mynull = 0;
static_assert(sizeof(mynull) == sizeof(void*), "sizeof(nullptr) is wrong");
