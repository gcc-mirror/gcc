// { dg-do run { target c++11 } }
// PR c++86610 lambda capture inside template

struct C
{
  int operator[](int)
  { return 1; }

  int operator[](int) const
  { return 0; } // Want this one
};

int q()
{
  C c;
  return [=] { return c[0]; }();
}

template <typename T>
int f()
{
  C c;
  T d;
  return [=] { return c[0]; }() 
    + [=] { return c[0] + d[0]; }();
}

int main()
{
  return q () + f<C>();
}
