// { dg-do run }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Nov 2002 <nathan@codesourcery.com>

// covariant returns. Fixed & virtual offset.

struct B1;
struct B2;
struct D;

struct B1
{
  virtual B1 *foo1 () {return this;}
  virtual B2 *foo2 (D *);
};

struct B2
{
  virtual B2 *baz1 () {return this;}
  virtual B1 *baz2 (D *);
};

struct Pad1 { virtual ~Pad1 (){}};
struct Pad2 { virtual ~Pad2 (){}};
struct Proxy1 : Pad1, B1 {};
struct Proxy2 : Pad2, B2 {};

struct D : virtual Proxy1, virtual Proxy2
{
  virtual D *foo1 () {return this;}
  virtual D *foo2 (D *d) {return d;}
  virtual D *baz1 () {return this;}
  virtual D *baz2 (D *d) {return d;}
};

B2 *B1::foo2 (D *d) {return d;}
B1 *B2::baz2 (D *d) {return d;}

int test (B1 *b1, B2 *b2, D *d)
{
  if (b1->foo1 () != b1)
    return 1;
  if (b2->baz1 () != b2)
    return 2;
  if (b1->foo2 (d) != b2)
    return 3;
  if (b2->baz2 (d) != b1)
    return 4;
  return 0;
}

int test (D *d)
{
  if (d->foo2 (d) != d)
    return 11;
  if (d->baz2 (d) != d)
    return 12;
  if (d->foo1 () != d)
    return 13;
  if (d->baz1 () != d)
    return 14;
  return 0;
}

int main ()
{
  D d;
  int r;
  
  if ((r = test (&d, &d, &d)))
    return r;
  if ((r = test (&d)))
    return r;
  return 0;
}
