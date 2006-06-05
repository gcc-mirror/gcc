// PR c++/27819
 
struct A
{
  static const char i = 1;
};

template<int> struct B
{
  static const int j = A::i;
  int x[int(j)];
};

B<0> b;
