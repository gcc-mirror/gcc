extern "C" void abort();

template <class T>
struct S1
{
  static void f();
};

template <>
void S1<int>::f() {}

struct S2
{
  template <class T>
  static void g(T);
};

template <>
void S2::g(double) {}

template <>
void S2::g<int>(int) {}

template <class T>
struct S3
{
  template <class U>
  static int h(U);
};

template <>
template <>
int S3<double>::h(int) { return 0; }

template <>
template <>
int S3<char>::h(int) { return 1; }

int main()
{
  S1<int>::f();
  S2::g(3.0);
  S2::g(7);

  if (S3<double>::h(7) != 0) 
    abort();
  if (S3<char>::h(7) != 1)
    abort();
}
