// { dg-do run { xfail *-*-* } }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Dec 2001 <nathan@nathan@codesourcery.com>

// PR 411

bool was_f_in_Bar_destroyed=false;

struct Foo
{
  ~Foo()
  {
    was_f_in_Bar_destroyed=true;
  }
};

struct Bar
{
  ~Bar()
  {
    throw 1;
  }
  
  Foo f;
};

int main()
{
  try
    {
      Bar f; 
    }
  catch(int i)
    {
      if(was_f_in_Bar_destroyed)
	{
	  return 0;
	}
    }
  return 1;
}
