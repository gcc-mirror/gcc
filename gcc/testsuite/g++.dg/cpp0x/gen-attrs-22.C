// PR c++/27648
// { dg-do compile { target c++11 } }

void f()
{
  static_cast<float *[[gnu::unused]]>(0);
}
