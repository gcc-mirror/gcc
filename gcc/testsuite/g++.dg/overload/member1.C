// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Sep 2002 <nathan@codesourcery.com>

struct X
{
  template<typename T> static void ProcessProxy ();
  typedef void (*Callback) ();
  void Process (Callback);
  
  template<typename T> void Process ()
  {
    Process (&ProcessProxy<T>);
  }
  
};

void foo (X *x)
{
  x->Process<int> ();
}
