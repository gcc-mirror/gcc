// { dg-do compile { target c++20 } }

using TD = int;

template<typename T>
concept C = requires () { typename TD; };

static_assert(C<int>, "");
