template <class T>
void f(T);

class C
{
  template <class T>
  friend void f(T);

  int i;
};


template <class T>
void f(T)
{
  C c;
  c.i = 3;
}


int main()
{
  f(7);
}
