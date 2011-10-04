// { dg-options -std=c++0x }

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
