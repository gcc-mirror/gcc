// PR c++/71728
// { dg-do compile }
// { dg-options "-std=gnu++14 -Wall" }

int
foo ()
{
  if (({ goto test; test: 1; }) != 1)
    return 1;
  return 2;
}
