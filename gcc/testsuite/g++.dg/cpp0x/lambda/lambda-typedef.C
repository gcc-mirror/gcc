// PR c++/54975
// { dg-do compile { target c++11 } }

template<typename T>
struct identity
{
  typedef T type;
};

template<typename T>
void f()
{
  typedef typename T::type A;
  int i = 42;
  int const &cri = i;
  int volatile &vri = i;
  [&]() {
    A const &x = cri;    // Problem here
    A volatile &y = vri; // Likewise
  };
}

int main()
{
  f<identity<int> >();
}
