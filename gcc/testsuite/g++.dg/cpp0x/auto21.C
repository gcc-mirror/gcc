// Origin PR c++/47208
// { dg-do compile { target c++11 } }

constexpr auto list = { }; // { dg-error "deducing from brace-enclosed initializer list requires #include <initializer_list>" }
static const int l = list.size();
