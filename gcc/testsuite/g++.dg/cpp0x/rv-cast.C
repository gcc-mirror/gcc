// { dg-options "-std=c++0x" }

void f(int i)
{
  int&& r = static_cast<int&&>(i);
}
