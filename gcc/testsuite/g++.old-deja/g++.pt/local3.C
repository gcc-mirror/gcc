extern "C" void abort();

template <class T>
void f(T)
{
  int j;

  j = 6;

  struct S {
    int i;
  };

  S s;

  s.i = j;

  if (s.i != 6)
    abort();
}


int main()
{
  f(7);
}
