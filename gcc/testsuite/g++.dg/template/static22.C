// PR c++/26266

template<typename> struct A
{
  static const int i = 1;
  static const int j = i;
  static const int k = int(j);
  int x[k];
};

A<char> a;
