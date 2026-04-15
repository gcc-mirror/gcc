// PR c++/124850
// { dg-do compile }

static union { int i; };
int &r = i;

int
foo ()
{
  return r;
}
