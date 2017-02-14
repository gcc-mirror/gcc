// PR c++/71630
// { dg-do compile { target c++14 } }

template <class T>
extern T pi;

int main()
{
  return pi<int>;
}
