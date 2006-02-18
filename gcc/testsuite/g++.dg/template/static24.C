template<typename> struct A;

template<> struct A<char>
{
  static const char i = 1;
};

template<typename T> struct B
{
  static const int j = A<T>::i;
  static const int k = int(j);
  int x[k];
};

B<char> b;
