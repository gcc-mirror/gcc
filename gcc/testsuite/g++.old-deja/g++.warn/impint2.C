// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 6 Mar 2000 <nathan@codesourcery.com>

// initialization to 'int' from to 'double' We expect consistent warnings
// whenever a float is implicitly truncated to int, make sure references
// don't confuse us, as Gerald Pfeifer <pfeifer@dbai.tuwien.ac.at> found out.

struct X
{
  X (int const &);
  X (int const &, int const &);
};

void foo (int const &);
void wibble (int const &);
void wibble (int const &, int const &);
void punk (int const & = 3.5f);        // { dg-warning "" } in passing
void rock ();
void rock (int const &, int  const & = 3.5f);       // { dg-warning "" } in passing

void fn ()
{
  X x2(3.5f);       // { dg-warning "" } float to int
  X x4(1, 3.5f);    // { dg-warning "" } float to int
  X x6(3.5f, 1);    // { dg-warning "" } float to int

  X y2 = 3.5f;      // { dg-warning "" } float to int

  int j2 (3.5f);    // { dg-warning "" } float to int

  int k2 = 3.5f;    // { dg-warning "" } float to int
  
  j2 = 3.5f;        // { dg-warning "" } float to int
  
  foo (3.5f);       // { dg-warning "" } float to int
  
  wibble (3.5f);    // { dg-warning "" } float to int
  wibble (1, 3.5f); // { dg-warning "" } float to int
  wibble (3.5f, 1); // { dg-warning "" } float to int
  
  punk ();          // { dg-warning "" } float to int
  rock (1);         // { dg-warning "" } float to int
}

// and make sure we really know when something's unsigned
void foo ()
{
  X x2(-1);
  X x4(1, -1);
  X x6(-1, 1);

  X y2 = -1;

  int j2 (-1);

  int k2 = -1;
  
  j2 = -1;
  
  foo (-1);
  
  wibble (-1);   
  wibble (1, -1);
  wibble (-1, 1);
  
}
