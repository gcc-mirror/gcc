// { dg-do run }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 12 Oct 2002 <nathan@codesourcery.com>

// From WindRiver SPR 80797
// We were inadvertently SAVE_EXPRing volatile arrays during delete[]

struct A
{
  A *ptr;
  static int ok;
  
  A () {ptr = this;}
  ~A () {ok = ptr == this;}
};
int A::ok = -1;

struct B
{
  B *ptr;
  static int ok;
  
  B () {ptr = this;}
  ~B () {ok = ptr == this;}
};
int B::ok = -1;

struct C
{
  A volatile a;
  B volatile b[1];

  C ();
};

C::C ()
{
  throw 1;
}

int main ()
{
  try
    {
      C c;
    }
  catch (...)
    {
      if (A::ok != 1)
	return 1;
      if (B::ok != 1)
	return 2;
      return 0;
    }
  return 3;
}
