// PR c++/103185
// { dg-do compile { target c++11 } }

using intarr = int[];
static_assert(__is_same(decltype(0[intarr{0}]), int&&), "");
