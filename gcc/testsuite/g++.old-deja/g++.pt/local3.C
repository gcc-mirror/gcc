extern "C" void abort();

template <class T>
void f(T)
{
  struct S {
    int i;
  };

  S s;

  s.i = 3;

  if (s.i != 3)
    abort();
}


int main()
{
  f(7);
}
