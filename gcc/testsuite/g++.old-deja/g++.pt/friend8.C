// { dg-do run  }
template <class T>
class C
{
  friend void f (C<T> c)
    {
      c.i = 3;
    }

  int i;
};


int main()
{
  C<int> ci;

  f(ci);
}
