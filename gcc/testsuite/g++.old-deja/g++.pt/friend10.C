// { dg-do run  }
template <class T>
void f(T);

template <class U>
class C
{
  template <class T>
  friend void f(T)
    {
      C<U> c;
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
  C<double> c;
  c.g();
}
