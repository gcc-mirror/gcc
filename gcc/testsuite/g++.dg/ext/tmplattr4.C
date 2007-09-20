// PR c++/7586
// { dg-do run }

template<typename T>
int f()
{
  typedef unsigned char type[sizeof (T)]
    __attribute((aligned(__alignof(T))));

  return __alignof (type);
}

int main()
{
  if (f<int>() == __alignof (int))
    return 0;
  else
    return 1;
}
