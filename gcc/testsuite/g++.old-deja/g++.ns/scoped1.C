// { dg-do assemble  }
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 10 Aug 2000 <nathan@codesourcery.com>

// Bug 354. We ICE'd before saying a namespace isn't an aggregate type.

namespace mlp
{
  struct base
  {
    void reset ();
  };
}

struct eo : mlp:: base
{
};

void foo (eo &ref)
{
  ref.mlp::base::reset ();
  ref.base::reset ();
  ref.reset ();
  ref.mlp::reset ();        // { dg-error "" } not an aggregate type
}
