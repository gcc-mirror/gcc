// { dg-do run  }
template <class T>
void f(T);

class C
{
  template <class T>
  friend void f(T)
    {
      C c;
      c.i = 3;
    }

public:

  void g()
    {
      f(3.0);
    }

  int i;
};

int main()
{
  f(7);
  C c;
  c.g();
}
