// { dg-do compile { target c++14 } }
// { dg-final { scan-assembler "_ZN1X1xIiEE" } }

struct X
{
  template <class T> static T x;
};

template <class T>
T X::x = T();

int main()
{
  int x = X::x<int>;
}
