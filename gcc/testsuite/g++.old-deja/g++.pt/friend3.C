// { dg-do assemble  }

template <class T>
void f(T);

class C
{
  friend void f<>(double);

  int i; // { dg-error "" } private
};


template <class T>
void f(T)
{
  C c;
  c.i = 3; // { dg-error "" } f<double> is a friend, this is f<int>.
}


int main()
{
  f(7);
}
