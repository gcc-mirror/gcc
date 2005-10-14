// { dg-do run }

// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 13 Oct 2005 <nathan@codesourcery.com>

// PR 23984:ICE
// Origin:  Andrew Pinski pinskia@gcc.gnu.org

struct B
{
  virtual void Foo ();
};

void B::Foo ()
{
}

struct D : virtual B
{
};

struct E
{
  B *ptr;
  
  E (B *);
};

static B *ptr;

E::E (B *ptr_)
  :ptr (ptr_)
{
}

struct G : D, E
{
  G ();
};

G::G ()
  : E (this)
{
}

int main ()
{
  G object;

  return object.ptr != &object;
}
