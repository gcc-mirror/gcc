// { dg-do compile }

int a, b;
typedef double (*NormFunc) (const int &);
int &
max (int &p1, int &p2)
{
  if (p1 < p2)
    return p2;
  return p1;
}

struct A
{
  int operator      () (int p1, int p2)
    {
      return max (p1, p2);
    }
};
template < class, class > double
norm_ (const int &)
{
  char c, d;
  A e;
  for (; a; a++)
    {
      b = e (b, d);
      b = e (b, c);
    }

  return 0.0;
}

void
norm ()
{
  static NormFunc f = norm_ < int, A >;
  f = 0;
}

