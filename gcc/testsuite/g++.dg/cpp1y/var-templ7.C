// { dg-do compile { target c++1y } }

template <class T> T var = 0;

int main()
{
  var<int> = 42;
}
