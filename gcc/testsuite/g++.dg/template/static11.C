// PR c++/19826

template<typename T> struct A
{
  static const T i = 1;
  char a[i];
};

