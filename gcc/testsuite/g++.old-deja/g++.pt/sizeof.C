// { dg-do run  }
extern "C" void abort();

template <int I>
int bar() { return I; }

template <class T>
int foo(T)
{
  return bar<sizeof(T) + 4>() + bar<sizeof(long) + 7>();
}


int main()
{
  if (foo(2) != sizeof(int) + 4 + sizeof(long) + 7)
    abort();
}
