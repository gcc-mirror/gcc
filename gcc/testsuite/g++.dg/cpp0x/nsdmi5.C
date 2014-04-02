// { dg-do compile { target c++11 } }

struct X
{
  int x = 5;
  int f() { return x; }
};
struct Y : X
{
  int y = this->x;
};
template <class T> struct Z : T
{
  int y = this->f();
};
int main()
{
  Y foo;
  Z<X> bar;
}
