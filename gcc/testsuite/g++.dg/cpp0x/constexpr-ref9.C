// { dg-do compile { target c++11 } }

int a[2] = { 1, 2 };

int main()
{
  auto &r = a;
  static_assert (&r[0] == &a[0], "");
}
