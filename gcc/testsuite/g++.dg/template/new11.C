// PR c++/54984
// { dg-do run }

int n = 1;

void* operator new(__SIZE_TYPE__)
{
  n = -1;
  return &n;
}

template <class T>
struct Foo
{
  Foo()
  : x(new int)
  {
    if (*x != -1)
      __builtin_abort();
  }

  int* x;
};

int main()
{
  Foo<float> foo;
}
