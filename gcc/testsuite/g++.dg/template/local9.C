// PR c++/61445

template <typename T> void f (T)
{
  struct A
  {
    struct B { B(); };
    void g () { B b; }
  };
}

int main()
{
  f(0);
}
