// { dg-do run  }
template <class T>
void f(T);

template <class U>
class C
{
  template <class T>
  friend void f(T);

  int i;
};


template <class T>
void f(T)
{
  C<T> c;
  c.i = 3;
}


int main()
{
  f(7);
}
