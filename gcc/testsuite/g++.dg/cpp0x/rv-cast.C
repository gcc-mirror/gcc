// { dg-do compile { target c++11 } }

void f(int i)
{
  int&& r = static_cast<int&&>(i);
}
