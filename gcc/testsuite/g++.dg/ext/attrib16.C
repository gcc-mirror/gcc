// { dg-do compile }
// Origin: <rguenth at tat dot physik dot uni-tuebingen dot de>
// PR c++/10479: use of non dependent expressions in attributes in templates

template <int i>
struct foo2 {
  float bar __attribute__((aligned(__alignof__(double))));
};
