// { dg-do compile { target c++14 } }

template <class T> T var = 0;

int main()
{
  var<int> = 42;
}
