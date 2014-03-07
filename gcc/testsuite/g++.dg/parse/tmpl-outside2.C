// PR c++/50080

template <typename T>
struct A
{
  template <typename U>
  struct B {};
};

template <typename T>
void test()
{
  typename A<T>::template B<int> b;
}

int main()
{
  typename A<double>::template B<int> b; // { dg-error "template|expected" "" { target { ! c++11 } } }
}
