// { dg-do run  }
template <int I>
int f()
{
  enum E { a = I };

  struct S {
    int g() {
      E e;
      e = a;
      return (int) e;
    }
  };

  S s;

  return s.g();
}


int main()
{
  if (f<7>() != 7)
    return 1;
  if (f<-3>() != -3)
    return 1;
  return 0;
}
