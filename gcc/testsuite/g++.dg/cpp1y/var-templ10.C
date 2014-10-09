// { dg-do compile { target c++14 } }

template <class T>
struct Y
{
  template <class U> static U x;
};

template <class T>
template <class U>
U Y<T>::x = U();

int main()
{
  int y = Y<int>::x<int>;
}
