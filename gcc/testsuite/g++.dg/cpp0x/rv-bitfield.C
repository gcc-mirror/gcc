// { dg-options -std=c++0x }

struct A
{
  int i : 1;
};

int main()
{
  A a;
  static_cast<int&&>(a.i);
}
