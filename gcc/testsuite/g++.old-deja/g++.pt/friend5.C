// { dg-do run  }
class C
{
  template <class T>
  friend void f(T)
    {
      C c;
      c.i = 3;
    }

  int i;
};


int main()
{
  f(C());
}
