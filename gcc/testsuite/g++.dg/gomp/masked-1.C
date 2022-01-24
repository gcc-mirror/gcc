// PR c++/103349
// { dg-do compile { target c++11 } }

int v;

void
foo (int x, int y)
{
  [=] ()
  {
#pragma omp masked
    v = x + y;
  } ();
}
