// PR c++/58761
// { dg-do compile { target c++11 } }

template <class T>
struct X
{
  int x = 42;
  int y = [this](){return this->x;}();
};

template <class T>
struct Y
{
  int x = 42;
  int y = [this](){return this->x;}();
  Y(int) {}
};

int main()
{
  X<int> x;
  Y<int> y(42);
}
