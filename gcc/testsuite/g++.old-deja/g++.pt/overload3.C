// { dg-do assemble  }

template <class T>
void g(T, T);

template <class T>
void g(int*, T);

struct S
{
  void f() const
    {
      g(X, X+3);
    }

  double X[3];
};

