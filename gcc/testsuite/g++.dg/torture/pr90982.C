// { dg-do compile }

template <int n> struct S
{
  long c[n];
  void f (S d)
    {
      for (int i = 2;; i++)
	c[i] &= d.c[i];
    }
};

template <int m> struct T:S<m/64>
{
  void operator &= (T d)
    { this -> f (d); }
};

void g (T<192> &d)
{
  T<192> v;
  d &= v;
}
