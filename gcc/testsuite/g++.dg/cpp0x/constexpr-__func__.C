// PR c++/55425
// { dg-do compile { target c++11 } }

constexpr const char* x() { return __func__; }
constexpr const char* y() { return __FUNCTION__; }
constexpr const char* z() { return __PRETTY_FUNCTION__; }
