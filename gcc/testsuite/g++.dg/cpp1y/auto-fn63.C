// PR c++/105779
// { dg-do compile { target c++14 } }

template<int>
struct struct1
{
  static auto apply() { return 1; }
};

int method(int(*f)());

int t = method(struct1<1>::apply);
