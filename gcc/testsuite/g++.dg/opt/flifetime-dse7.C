// { dg-options "-O3 -flifetime-dse" }
// { dg-do run }

template <class T>
void f()
{
  T t = 42;
  t.~T();
  if (t == 42) __builtin_abort();
}

int main()
{
  f<int>();
}

