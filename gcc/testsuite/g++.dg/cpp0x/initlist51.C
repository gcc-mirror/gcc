// PR c++/47184
// { dg-options -std=c++11 }

struct S
{
  int a;
};
struct T
{
  T(S s) {}
};
int main()
{
  T t(S{1});
}
