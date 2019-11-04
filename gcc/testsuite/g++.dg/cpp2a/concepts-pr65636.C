// { dg-do compile { target c++2a } }

using TD = int;

template<typename T>
concept C = requires () { typename TD; };

static_assert(C<int>, "");
