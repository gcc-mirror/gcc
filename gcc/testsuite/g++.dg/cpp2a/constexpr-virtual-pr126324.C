// PR c++/126324
// { dg-do compile { target c++20 } }

struct B
{
  virtual constexpr B *clone(bool null)
  {
    return null ? nullptr : this;
  }
};

struct C
{
  virtual void dummy() { }
};

struct D : B
{
  constexpr D *clone(bool null) override
  {
    return null ? nullptr : this;
  }
};

struct E : C, B
{
  int calls = 0;

  constexpr E *clone(bool null) override
  {
    ++calls;
    return null ? nullptr : this;
  }
};

constexpr bool test()
{
  D d;
  B *b = &d;
  D *direct = d.clone (false);
  B *nonnull = b->clone (false);
  B *null = b->clone (true);

  E e;
  B *eb = &e;
  B *enonnull = eb->clone (false);
  B *enull = eb->clone (true);

  return (direct == &d && nonnull == b && null == nullptr
	  && enonnull == eb && enull == nullptr && e.calls == 2);
}

static_assert (test ());
