// { dg-do assemble  }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Feb 2001 <nathan@codesourcery.com>

// Bug 2117. A conversion op to reference type created a temporary, even
// when bound to another reference.

struct Abstract
{
  virtual void Foo () = 0;
};

struct Proxy
{
  operator Abstract & ();
  Abstract &Convert ();
};

void Baz (Abstract &);

void Foo ()
{
  Proxy proxy;
  
  Baz (proxy);
  Baz (proxy.Convert ());
}
