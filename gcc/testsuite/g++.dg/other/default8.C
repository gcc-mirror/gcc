// PR c++54485

template<typename T>
class K1
{
  int fn(int, int);
  int gn(int, int);
};

template<typename T>
int K1<T>::fn (int a, int b = 3)      // { dg-error "default arguments" }
{
  return a - b;
}

template<typename T>
int K1<T>::gn (int a = 1, int b = 3)  // { dg-error "default arguments" }
{
  return a - b;
}

template<typename T>
class K2
{
  template<typename U>
  int fn(int, int);
  template<typename U>
  int gn(int, int);
};

template<typename T>
template<typename U>
int K2<T>::fn (int a, int b = 3)  // { dg-error "default arguments" }
{
  return a - b;
}

template<typename T>
template<typename U>
int K2<T>::gn (int a = 1, int b = 3)  // { dg-error "default arguments" }
{
  return a - b;
}
