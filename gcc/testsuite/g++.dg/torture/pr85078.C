typedef __builtin_va_list a;

class b
{
public:
  virtual void c (int, const char *, a &);
  char d;
  void m_fn2 ()
  {
    a a;
    c (2, &d, a);
  }
};

class e:b
{
  virtual void f ()
  {
  }
  void c (int, const char *, a &);
};

class g
{
protected:
  b h;
};

class i:g
{
  int j ();
};

int
i::j ()
{
  h.m_fn2 ();
  return 0;
}

