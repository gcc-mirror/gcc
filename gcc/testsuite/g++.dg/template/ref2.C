// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Jan 2005 <nathan@codesourcery.com>

// PR 19298: Rejects legal
// Origin: Andrew Pinski <pinskia@gcc.gnu.org>

struct t
{
  void f() const;
};

template <typename _Tp>
struct A
{
 static t const&  c;
};

template <typename _Tp>
void g(void)
{
  A<_Tp>::c.f();
}

void h(void)
{
  g<int>();
}
