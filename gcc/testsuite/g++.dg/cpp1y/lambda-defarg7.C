// PR c++/82053
// { dg-do compile { target c++14 } }

template<class T>
int fn() { return 42; }

template<class T>
auto lam = [](int = fn<T>()){};

int main()
{
  lam<int>();
}
