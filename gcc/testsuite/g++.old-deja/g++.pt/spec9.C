// { dg-do assemble  }
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
{             // { dg-error "" } specialization of f<int>(int) after instantiation
  return 1;
}


