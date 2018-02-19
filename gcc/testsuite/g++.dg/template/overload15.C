// PR c++79064 - Cannot overload member function templates on type of literal
// { dg-do compile }

template <unsigned N>
void f (char (*)[0u - 1 > N ? 1 : 7]);

template <unsigned N>
void f (char (*)[0u - 1ll > N ? 1 : 7]);

void f ()
{
  char x[1], y[7];

  f<0>(&x);
  f<0>(&y);
}
