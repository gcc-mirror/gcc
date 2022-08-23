/* PR middle-end/104761 - bogus -Wdangling-pointer with cleanup and infinite loop
   { dg-do compile }
   { dg-options "-O -Wall" } */

typedef struct { int i; } S;

void f (S **);

int g (int);

void nowarn (int x)
{
  S s = { 0 };

  __attribute__((__cleanup__ (f))) S *p = 0;

  if (x)
    {
      g (s.i);                // { dg-bogus "-Wdangling-pointer" }
      for ( ; ; );
    }
}
