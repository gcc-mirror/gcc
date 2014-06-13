// PR c++/59483
// { dg-do compile { target c++11 } }

struct X
{
protected:
  int i;
};

struct Y : X
{
  Y()
  {
    [&]{ X::i = 3; }();
  }
};

template <class T>
struct Y2 : T
{
  Y2()
  {
    [&]{ T::i = 3; }();
  }
};

int main()
{
  Y y;
  Y2<X> y2;
}
