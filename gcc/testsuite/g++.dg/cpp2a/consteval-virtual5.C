// { dg-do compile { target c++20 } }

struct B1;
struct B2;
struct D;

struct B1
{
  virtual consteval const B1 *foo1 () const {return this;}
  virtual consteval const B2 *foo2 (const D *) const;
};
struct B2
{
  virtual consteval const B2 *baz1 () const {return this;}
  virtual consteval const B1 *baz2 (const D *) const;
};

struct D : public B1, B2
{
  virtual consteval const D *foo1 () const {return this;}
  virtual consteval const D *foo2 (const D *d) const {return d;}
  virtual consteval const D *baz1 () const {return this;}
  virtual consteval const D *baz2 (const D *d) const {return d;}
};

consteval const B2 *B1::foo2 (const D *d) const {return d;}
consteval const B1 *B2::baz2 (const D *d) const {return d;}

consteval int
test (const B1 *b1, const B2 *b2, const D *d)
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

consteval int
test (const D *d)
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

constexpr D d;
constexpr auto e = test (&d, &d, &d);
constexpr auto f = test (&d);
static_assert (e == 0);
static_assert (f == 0);
