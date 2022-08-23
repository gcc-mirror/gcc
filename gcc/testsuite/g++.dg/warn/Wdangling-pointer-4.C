/* PR middle-end/104761 - bogus -Wdangling-pointer with cleanup and infinite loop
   { dg-do compile }
   { dg-options "-O -Wall -fno-exceptions" } */

struct S { int i; };

struct X { ~X (); };

void g (int);

void test (int i)
{
  S s = { 0 };

  X x;

  if (i)
    {
      g (s.i);                // { dg-bogus "-Wdangling-pointer" }
      for ( ; ; );
    }
}
