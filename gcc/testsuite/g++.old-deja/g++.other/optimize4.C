// { dg-do assemble  }
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Jul 2001 <nathan@codesourcery.com>

// Bug 3543. We forgot to resolve an OFFSET_REF


struct Writeable {
  bool blocking_mode;
};


struct Pipe : Writeable {
  void ewrite();

  void set_write_blocking ()
  {
    if (Writeable::blocking_mode);
  }
};

void Pipe::ewrite()
{
  set_write_blocking();
}

void ewrite(Pipe &p)
{
  p.set_write_blocking();
}
