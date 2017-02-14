// PR middle-end/78201
// { dg-do compile }
// { dg-options "-O2" }

struct B { long d (); } *c;
long e;

void
foo ()
{
  char a[e] = "";
  c && c->d();
}
