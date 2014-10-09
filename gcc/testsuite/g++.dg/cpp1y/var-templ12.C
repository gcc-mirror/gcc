// { dg-do compile { target c++14 } }
// { dg-options "-Wall" }

template <class T> T x;
template <> int x<int> = 0;

int main()
{
  return x<int>;
}
