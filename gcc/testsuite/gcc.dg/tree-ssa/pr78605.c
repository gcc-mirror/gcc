/* PR middle-end/78605 - bogus -Wformat-overflow=1 with %f
   { dg-do compile }
   { dg-options "-O2 -Wall -Wextra -Wformat-overflow=1" } */

char d[10];

int f (int i)
{
  return __builtin_sprintf (d, "%i %i", i, i);
}

int g (float f)
{
  return __builtin_sprintf (d, "%.2f %.2f", f, f);
}
