// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Feb 2000 <nathan@codesourcery.com>

// derived from a bug report by Johan Kuipers <j.kuipers@chello.nl>
// initialization to 'int' from to 'double' We expect consistent warnings
// whenever a float is implicitly truncated to int

struct X
{
  X (int);
  X (int, int);
};

void foo (int);
void wibble (int);
void wibble (int, int);
void punk (int = 3.5);
void rock ();
void rock (int, int = 3.5);

void fn ()
{
  X x1(3.5);        // WARNING - double to int
  X x2(3.5f);       // WARNING - float to int
  X x3(1, 3.5);     // WARNING - double to int
  X x4(1, 3.5f);    // WARNING - float to int
  X x5(3.5, 1);     // WARNING - double to int
  X x6(3.5f, 1);    // WARNING - float to int

  X y1 = 3.5;       // WARNING - double to int
  X y2 = 3.5f;      // WARNING - float to int

  int j1 (3.5);     // WARNING - double to int
  int j2 (3.5f);    // WARNING - float to int

  int k1 = 3.5;     // WARNING - double to int
  int k2 = 3.5f;    // WARNING - float to int
  
  j1 = 3.5;         // WARNING - double to int
  j2 = 3.5f;        // WARNING - float to int
  
  foo (3.5);        // WARNING - double to int
  foo (3.5f);       // WARNING - float to int
  
  wibble (3.5);     // WARNING - double to int
  wibble (3.5f);    // WARNING - float to int
  wibble (1, 3.5);  // WARNING - double to int
  wibble (1, 3.5f); // WARNING - float to int
  wibble (3.5, 1);  // WARNING - double to int
  wibble (3.5f, 1); // WARNING - float to int
  
  punk ();          // WARNING - double to int
  rock (1);         // WARNING - double to int
}

