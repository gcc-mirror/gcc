// { dg-do assemble  }
// { dg-options "" }
// Origin: Mark Mitchell <mark@codesourcery.com>

void *vp;

void f ()
{
  int i = (*((int (*)[i]) vp))[0];
}
