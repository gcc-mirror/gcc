// PR 8327

template <class T>
class X
{
  static const int a = 5;

  static T b[a];
};

template <class T> T X<T>::b[X::a];

