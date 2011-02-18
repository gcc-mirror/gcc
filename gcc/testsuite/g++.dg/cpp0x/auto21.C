// Origin PR c++/47208
// { dg-options "-std=c++0x" }

constexpr auto list = { }; // { dg-error "deducing from brace-enclosed initializer list requires #include <initializer_list>" }
static const int l = list.size();
