template <class T> T* create ()
{
  return new T;
}

template <class T> T* create2()
{
  return create<T>();
}

int main()
{
  int *p = create2<int>();
}
