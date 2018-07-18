// { dg-do compile }
int a;
class d
{
public:
  virtual unsigned c ();
};
class e : public d
{
};
class i
{
  void h ();

public:
  void
  operator= (e *f)
  {
    j<int>::c (f);
    h ();
  }
  template <class> struct j
  {
    static void
    c (e *g)
    {
      g->c ();
    }
  };
};
class k;
class l
{
public:
  l (int);
  k *operator-> ();
};
class m final : e
{
  unsigned c ();
};
class k
{
public:
  virtual int o (e *) = 0;
};
class H : d, k
{
  int o (e *);
  i n;
};
unsigned
m::c ()
{
  l b = 0;
  b->o (this);
  return a;
}
int
H::o (e *p)
{
  n = p;
  return a;
}


