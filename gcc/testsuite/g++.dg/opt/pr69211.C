// PR c++/69211
// { dg-do compile }

int a, b;

int
foo ()
{
  return (a & 5UL | (b = 4, 4L)) > 4;
}
