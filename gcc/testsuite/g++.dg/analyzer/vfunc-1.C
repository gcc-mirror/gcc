struct base
{
  virtual int fn () const;
};
struct sub : public base
{
  int fn () const;
};

int
test_1 (base *p)
{
  return p->fn ();
}
