// PR c++/66619
// { dg-do compile { target c++14 } }

int y;
template<class T> T val1 = y;
auto&& x1 = val1<int&>;

template<class T> T val2 = 0;
auto&& x2 = val2<int&&>;
