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
  C<int> c;
  c.i = 3;
}


int main()
{
  f(7);
}
