extern "C" void abort();

template <class T>
struct S
{
  template <int i>
  int f(int j) { abort(); return 0; }
};

template <>
template <>
int S<double>::f<7>(int j) { return j + 7; }

template <>
template <>
int S<double>::f<8>(int j) { return j + 8; }

int main()
{
  S<double> s;

  if (s.template f<7>(3) != 10)
    abort();

  if (s.template f<8>(3) != 11)
    abort();
}
