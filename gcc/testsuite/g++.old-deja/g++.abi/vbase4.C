// { dg-do run  }
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 9 Jun 2001 <nathan@codesourcery.com>

// Bug 3089. We ICE'd in construction vtables.

int failed;

void fail (int val)
{
  if (!failed)
    failed = val;
}

struct A
{
  virtual ~A();
  A ();
  virtual void check (void *whole, void *base);
};

A::A ()
{
  check (this, this);
}
A::~A ()
{
  check (this, this);
}

void A::check (void *whole, void *base)
{
  if (dynamic_cast <void *> (this) != whole)
    fail (1);
  else if (this != base)
    fail (2);
}

struct B
{
  virtual ~B ();
  B ();
  virtual void check (void *whole, void *base);
};

B::B ()
{
  check (this, this);
}
B::~B ()
{
  check (this, this);
}
void B::check (void *whole, void *base)
{
  if (dynamic_cast <void *> (this) != whole)
    fail (3);
  else if (this != base)
    fail (4);
}

struct C : virtual public B, virtual public A
{
  virtual ~C ();
  C ();
  virtual void check (void *whole, void *base);
};
C::C ()
{
  check (this, this);
}
C::~C ()
{
  check (this, this);
}
void C::check (void *whole, void *base)
{
  if (dynamic_cast <void *> (this) != whole)
    fail (5);
  else if (this != base)
    fail (6);
  A::check (whole, static_cast <A *> (this));
  B::check (whole, static_cast <B *> (this));
}

struct D : virtual public A
{
  virtual ~D ();
  D ();
  virtual void check (void *whole, void *base);
};
D::D ()
{
  check (this, this);
}
D::~D ()
{
  check (this, this);
}
void D::check (void *whole, void *base)
{
  if (dynamic_cast <void *> (this) != whole)
    fail (5);
  else if (this != base)
    fail (6);
  A::check (whole, static_cast <A *> (this));
}

struct E : virtual public C, virtual public D
{
  virtual ~E ();
  E ();
  virtual void check (void *whole, void *base);
};
E::E ()
{
  check (this, this);
}
E::~E ()
{
  check (this, this);
}
void E::check (void *whole, void *base)
{
  if (dynamic_cast <void *> (this) != whole)
    fail (5);
  else if (this != base)
    fail (6);
  C::check (whole, static_cast <C *> (this));
  D::check (whole, static_cast <D *> (this));
}

struct F : virtual public E
{
  virtual ~F ();
  F ();
  virtual void check (void *whole, void *base);
};
F::F ()
{
  check (this, this);
}
F::~F ()
{
  check (this, this);
}
void F::check (void *whole, void *base)
{
  if (dynamic_cast <void *> (this) != whole)
    fail (5);
  else if (this != base)
    fail (6);
  E::check (whole, static_cast <F *> (this));
}

int main ()
{
  A a;
  B b;
  C c;
  D d;
  E e;
  F f;
  
  return failed;
}
