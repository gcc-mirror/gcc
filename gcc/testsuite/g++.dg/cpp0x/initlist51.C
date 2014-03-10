// PR c++/47184
// { dg-do compile { target c++11 } }

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
