// PR c++/26266

template<typename> struct A
{
  static const int i = 1;
};

template<typename> struct B
{
  static const int j = A<char>::i;
  static const int k = int(j);
  int x[k];
};

B<char> b;
