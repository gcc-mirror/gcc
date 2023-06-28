// PR c++/100372
// { dg-do compile { target c++14 } }

template <bool> using enable_if_t = int;
template <template <class> class> bool has_P_match_v;
template <template <class> class... List> enable_if_t<has_P_match_v<List...>> a;
