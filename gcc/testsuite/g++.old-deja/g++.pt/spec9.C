extern "C" void abort();

template <class T>
inline int f(T t) 
{
  return 0;
}

int main()
{
  if (!f(3))
    abort();
}

template <>
int f(int i) 
{             // ERROR - specialization of f<int>(int) after instantiation
  return 1;
}


