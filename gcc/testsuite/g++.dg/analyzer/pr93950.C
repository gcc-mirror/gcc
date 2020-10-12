// { dg-do compile { target c++11 } }

struct d
{
  struct e
  {
    int f;
    int *g;
  };
  void h (e * i)
  {
    void *j = nullptr; // { dg-bogus "NULL" }
    i->f = *i->g; // { dg-warning "dereference of NULL 'i'" }
  }
  virtual void c (int, int)
  {
    int *j = nullptr;
    h (nullptr);
  }
};

void
foo ()
{
  d ();
}
