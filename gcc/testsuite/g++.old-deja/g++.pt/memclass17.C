// Build don't link:

template <class T> struct S 
{
  template <class U> struct I 
  {
  };

  S();
  S(S& s);
  S(I<T>);

  template <class U> operator I<U>();
};

S<int> f();
void g(S<int>);

void h()
{
  g(f());
}
