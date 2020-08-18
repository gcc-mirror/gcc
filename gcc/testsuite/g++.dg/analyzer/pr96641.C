struct uh {
  virtual void
  sx ();
};

struct iz : uh {
  virtual void
  sx ()
  {
    sx ();
  }
};

void
a2 ()
{
  iz ().sx ();
}
