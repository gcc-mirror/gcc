// { dg-do compile }

int check;

template<typename T> void foo()
{
  check |= sizeof(T);
}

template<typename T>
void bar(void)
{
  #pragma omp parallel if (0)
    foo<T>();
}

int main()
{
  bar<char>();
  bar<short>();
  if (check != (sizeof(char) | sizeof(short)))
    __builtin_trap ();
  return 0;
}
