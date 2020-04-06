// PR c++/94339
// { dg-do compile }

unsigned a;
void bar ();

unsigned
foo (bool x)
{
  return x ? bar (), -1 : a;
}
