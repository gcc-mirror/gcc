// { dg-do link  }

template <class T>
int f(int (*fn)(T))
{
  return (*fn)(3);
}

struct S {
  static int g(int) { return 1; }
  static void g();

  int h();
};

int S::h()
{
  return f(&g);
}


int main()
{
  S s;
  if (s.h () != 1)
    return 1;
}
