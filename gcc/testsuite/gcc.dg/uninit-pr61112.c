/* PR tree-optimization/61112 - repeated conditional triggers false-positive
   -Wmaybe-uninitialized warning
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

int p;

void foo_c0 (int x, int y, int z)
{
  int w;
  if (x)
    w = z;
  if (y)
    w = __LINE__;

  if (x || y)
    p = w;          // { dg-bogus "-Wmaybe-uninitialized" }
}


void foo_c5_1_1 (int x, int y, int z, int a)
{
  int w;
  if (x)
    w = z;
  if (y)
    w = __LINE__;
  if (a)
    w = __LINE__;

  if (x || y || a)
    p = w;          // { dg-bogus "-Wmaybe-uninitialized" "pr61112" { xfail *-*-* } }
}

void foo_c5_1_2 (int x, int y, int z, int a)
{
  int w;
  if (x)
    w = z;
  if (y)
    w = __LINE__;
  if (a)
    w = __LINE__;

  if (x || a || y)
    p = w;          // { dg-bogus "-Wmaybe-uninitialized" "pr61112" { xfail *-*-* } }
}

void foo_c5_1_3 (int x, int y, int z, int a)
{
  int w;
  if (x)
    w = z;
  if (y)
    w = __LINE__;
  if (a)
    w = __LINE__;

  if (a || x || y)
    p = w;          // { dg-bogus "-Wmaybe-uninitialized" "pr61112" { xfail *-*-* } }
}

void foo_c5_2 (int x, int y, int z, int a)
{
  int w;
  if (x)
    w = __LINE__;
  if (y)
    w = z;
  if (a)
    w = __LINE__;

  if (x || y || a)
    p = w;          // { dg-bogus "-Wmaybe-uninitialized" }
}

void foo_c5_3 (int x, int y, int z, int a)
{
  int w;
  if (x)
    w = __LINE__;
  if (y)
    w = __LINE__;
  if (a)
    w = z;

  if (x || y || a)
    p = w;          // { dg-bogus "-Wmaybe-uninitialized" }
}
