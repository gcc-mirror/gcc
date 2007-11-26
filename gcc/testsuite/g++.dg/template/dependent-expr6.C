// { dg-do compile }

// Copyright 2007 Free Software Foundation
// Contributed by Andreas Krebbel <Andreas.Krebbel@de.ibm.com>

// PR C++ 34081 ICE

class Foo;

template < class Foo > class Bar
{
  enum Status
  { OK, NO };

  enum Status getStatus ()
  {
    return status;
  }

  Status status;
};
