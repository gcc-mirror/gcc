// PR c++/93400
// { dg-do compile { target concepts } }

template <typename> bool a = true;
template <typename i> concept b = a<i>;
template <int> struct f { template <b c> friend auto g(c, f); };
auto d = f<1>{};
auto e = f<0>{};
