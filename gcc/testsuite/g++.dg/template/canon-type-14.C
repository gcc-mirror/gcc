// PR c++/94632
// { dg-do compile { target c++11 } }

template <bool> struct b;
template <typename> class c {
  template <typename f> static void d(f e, b<decltype(e)::k>);
};
template class c<int>;
