// Build don't link:

template <class T>
void f(T);

class C
{
  friend void f<>(double);

  int i; // ERROR - private
};


template <class T>
void f(T)
{
  C c;
  c.i = 3; // ERROR - f<double> is a friend, this is f<int>.
}


int main()
{
  f(7);
}
