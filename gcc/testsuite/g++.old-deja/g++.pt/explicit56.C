// { dg-do run  }
template <class T> T* create ();

template <class T> T* create2()
{
  return create<T>();
}

template <class T> T* create ()
{
  return new T;
}

int main()
{
  int *p = create2<int>();
}
