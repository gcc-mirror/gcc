extern "C" void abort();

template <class T>
struct S {};

S<int> si;

template <class T>
int f(T t)
{
  struct S { 
    int g(int i) { return i + 2; }
  };

  S s;

  return s.g(t) + s.g(t);
}


int main()
{
  if (f(3) != 10)
    abort();
}
