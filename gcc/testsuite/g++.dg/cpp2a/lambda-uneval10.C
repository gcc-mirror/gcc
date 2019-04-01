// PR c++/89421

template <int I = []{return 1;}()> // { dg-message "lambda" "" { target c++17_down } }
struct B
{
  static const int i = I;
};

#if __cplusplus > 201703L
B<> b;
static_assert (b.i == 1);
#endif
