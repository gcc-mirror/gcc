// { dg-do assemble  }

int i;
int j;

struct S {
  int operator()(int)
    {
      i = 1;
      return i;
    }

  typedef int I;

  void f() {
    for (S I; false; )
      ;
    int k = I(3);
  }
};

typedef int J;

struct T {
  int operator()(int)
    {
      j = 1;
      return j;
    }

  void f() {
    for (T J; false; )
      ;
    int k = J(3);
  }
};

int main()
{
  S s;
  s.f();
  return 2 * i + j;
}
