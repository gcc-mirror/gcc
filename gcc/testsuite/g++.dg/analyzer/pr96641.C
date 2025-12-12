struct uh {
  virtual void
  sx ();
};

struct iz : uh {
  virtual void
  sx ()
  {
    sx (); /* { dg-warning "infinite recursion" } */
  }
};

void
a2 ()
{
  iz ().sx ();
}
