extern "C" void abort();

template <class T>
struct S
{
  template <class U>
  int f(U u);
};

template <class T>
template <>
int S<T>::f(int i) { return 1; }

int main()
{
  S<char> sc;

  if (sc.f(3) != 1)
    abort();
}
