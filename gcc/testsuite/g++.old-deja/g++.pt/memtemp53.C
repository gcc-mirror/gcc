// Build don't run:
// GROUPS passed templates membertemplates
template<int N>
struct I {
};

template<class T>
struct A {

  int r;

  template<class T1, class T2>
  void operator()(T1, T2)
  { r = 0; }

  template<int N1, int N2>
  void operator()(I<N1>, I<N2>)
  { r = 1; }
};

int main()
{
    A<float> x;
    I<0> a;
    I<1> b;

    x(a,b);
    if (x.r != 1)
        return 1;

    x(float(), double());
    if (x.r != 0)
        return 1;

    return 0;
}
