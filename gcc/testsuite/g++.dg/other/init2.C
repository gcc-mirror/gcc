// { dg-do run }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 20 Dec 2001 <nathan@nathan@codesourcery.com>

// PR 160. Wrong code emitted for some reference initializers.

void Foo ()
{
}

int fail;

class C
{
  public:
  int m;
  int &r;
  
  C () ;
};

C::C ()
  : m (1), r ((Foo (), m))
{
  m = 10;
  
  if (r != m)
    fail = 1;
  else if (&m != &r)
    fail = 2;
}
int main ()
{
  int m (1);
  int &r ((Foo (),m));

  m = 10;
  if (r != m)
    fail = 3;
  else if (&r != &m)
    fail = 4;

  if (!fail)
    {
      C c;
    }
  return fail;
}
